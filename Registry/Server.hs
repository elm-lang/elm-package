{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Registry.Server where

import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Control.Monad.Error
import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import GHC.Conc

import qualified Get.Utils as GUtils
import qualified Registry.Utils as Utils
import qualified Model.Name as N
import qualified Model.Version as V
import qualified Registry.Generate.Listing as Listing
import qualified Registry.Generate.Docs as Docs
import qualified Registry.Generate.Html as Html

import Snap
import Snap.Util.FileServe
import Snap.Util.FileUploads

import qualified Language.Elm as Elm

data Flags = Flags
  { port :: Int
  } deriving (Data,Typeable,Show,Eq)

flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  }

-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  getRuntimeAndDocs
  setupRootFiles
  createDirectoryIfMissing True Utils.libDir
  cargs <- cmdArgs flags
  httpServe (setPort (port cargs) defaultConfig) $
      ifTop (serveFile "public/Home.html")
      <|> route [ ("catalog"  , catalog)
                , ("versions" , versions)
                , ("register" , register)
                , ("metadata" , metadata)
                , ("resources", serveDirectoryWith directoryConfig "resources")
                ]
      <|> serveDirectoryWith directoryConfig "public"
      <|> do modifyResponse $ setResponseStatus 404 "Not found"
             serveFile "public/Error404.html"


getRuntimeAndDocs :: IO ()
getRuntimeAndDocs = do
  BS.writeFile "resources/elm-runtime.js" =<< BS.readFile =<< Elm.runtime
  BS.writeFile "resources/docs.json" =<< BS.readFile =<< Elm.docs

setupRootFiles :: IO ()
setupRootFiles =
    do result <- runErrorT generate
       case result of
         Right _ -> return ()
         Left err -> do hPutStrLn stderr err
                        exitFailure
    where
      generate = mapM Html.generateSrc $ map (\name -> "src/" ++ name ++ ".elm") elms
      elms = ["Error404","Catalog","Home"]

catalog :: Snap ()
catalog = 
    ifTop (serveFile "public/Catalog.html")
    <|> routeLocal [ (":name/:version", serveLibrary) ]
  where
    serveLibrary :: Snap ()
    serveLibrary = do
      request <- getRequest
      let directory = "public" ++ BSC.unpack (rqContextPath request)
      when (List.isInfixOf ".." directory) pass
      exists <- liftIO $ doesDirectoryExist directory
      when (not exists) pass
      ifTop (serveFile (directory </> "index.html")) <|> serveModule request

    serveModule :: Request -> Snap ()
    serveModule request = do
      let path = BSC.unpack $ BS.concat
                 [ "public", rqContextPath request, rqPathInfo request, ".html" ]
      when (List.isInfixOf ".." path) pass
      exists <- liftIO $ doesFileExist path
      when (not exists) pass
      serveFile path
              
  
directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexGenerator = defaultIndexGenerator defaultMimeTypes indexStyle
    , mimeTypes = Map.insert ".elm" "text/plain" defaultMimeTypes
    }

indexStyle :: BS.ByteString
indexStyle =
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \div.footer { display:none; }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"

register :: Snap ()
register =
  do nameAndVersion <- getNameAndVersion
     case nameAndVersion of
       Nothing -> error404 "Invalid library name or version number."
       Just (name,version) -> do
         let directory = Utils.libraryVersion name version
         exists <- liftIO $ doesDirectoryExist directory
         if exists
         then error404 "That version has already been registered."
         else do
           handleFileUploads "/tmp" defaultUploadPolicy perPartPolicy (handler directory)
           result <- liftIO $ runErrorT $ Docs.generate directory
           case result of
             Left err ->
                 do writeBS $ BSC.pack err
                    httpError 500 "Internal Server Error"
             Right () ->
                 writeBS "Registered successfully!"
  where
    perPartPolicy info
        | okayPart "docs" info || okayPart "deps" info = allowWithMaximumSize $ 2^(19::Int)
        | otherwise = disallow

    okayPart field part =
        partFieldName part == field
        && partContentType part == "application/json"

    handler dir parts =
     do case parts of
          [(info1, Right temp1), (info2, Right temp2)]
              | okayPart "docs" info1 && okayPart "deps" info2 ->
                   do copy dir temp1 Utils.json
                      copy dir temp2 GUtils.depsFile
              | okayPart "docs" info2 && okayPart "deps" info1 ->
                   do copy dir temp2 Utils.json
                      copy dir temp1 GUtils.depsFile

          _ -> do
            mapM (writeError . snd) parts
            error404' msg

    writeError = either (writeText . policyViolationExceptionReason) (const (return ()))
    msg = "Files " ++ Utils.json ++ " and " ++ GUtils.depsFile ++ " were not uploaded."

    copy directory temp path =
        do let permanent = directory </> path
           liftIO $ createDirectoryIfMissing True directory
           liftIO $ BS.writeFile permanent =<< BS.readFile temp

versions :: Snap ()
versions = do
  library <- getParam "library"
  case N.fromString . BSC.unpack =<< library of
    Nothing -> error404 "The request arguments are not well-formed."
    Just name ->
        do let path = Utils.library name
           exists <- liftIO $ doesDirectoryExist path
           versions <- case exists of
                         False -> return Nothing
                         True  -> do contents <- liftIO $ getDirectoryContents path
                                     return $ Just $ Maybe.mapMaybe V.fromString contents
           writeLBS (Binary.encode versions)

metadata :: Snap ()
metadata =
  do nameAndVersion <- getNameAndVersion
     case uncurry Utils.libraryVersion <$> nameAndVersion of
       Nothing -> error404 "Invalid library name or version number."
       Just directory -> do
         exists <- liftIO $ doesDirectoryExist directory
         if exists then serveFile (directory </> Utils.json)
                   else error404 "That library and version is not registered."

getNameAndVersion :: Snap (Maybe (N.Name, V.Version))
getNameAndVersion =
  do lib <- getParam "library"
     ver <- getParam "version"
     return $ (,) <$> (N.fromString . BSC.unpack =<< lib)
                  <*> (V.fromString . BSC.unpack =<< ver)

error404' :: String -> Snap ()
error404' msg =
    writeBS (BSC.pack msg) >> httpError 404 "Not Found"

error404 :: BSC.ByteString -> Snap ()
error404 msg =
    writeBS msg >> httpError 404 "Not Found"

httpError :: Int -> BSC.ByteString -> Snap ()
httpError code msg = do
  modifyResponse $ setResponseStatus code msg
  finishWith =<< getResponse
