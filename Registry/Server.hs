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
import System.FilePath
import System.Directory
import GHC.Conc

import qualified Registry.Utils as Utils
import qualified Model.Name as N
import qualified Model.Version as V
import qualified Registry.Generate.Docs as Docs

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
  createDirectoryIfMissing True Utils.libDir
  cargs <- cmdArgs flags
  httpServe (setPort (port cargs) defaultConfig) $
      route [ ("libraries/:name/:version", libraries)
            , ("versions"                , versions)
            , ("register"                , register)
            , ("metadata"                , metadata)
            ]
      <|> serveDirectoryWith directoryConfig "public"
      <|> serveDirectoryWith directoryConfig "resources"
      <|> error404 "Could not find that."

getRuntimeAndDocs :: IO ()
getRuntimeAndDocs = do
  BS.writeFile "resources/elm-runtime.js" =<< BS.readFile =<< Elm.runtime
  BS.writeFile "resources/docs.json" =<< BS.readFile =<< Elm.docs

libraries :: Snap ()
libraries =
    do request <- getRequest
       let directory = "public" ++ BSC.unpack (rqContextPath request)
       when (List.isInfixOf ".." directory) pass
       exists <- liftIO $ doesDirectoryExist directory
       when (not exists) pass
       ifTop serveIndex <|> serveModule request
    where
      serveIndex = do
        writeBS "there should be an index file here at some point"

      serveModule request = do
        let path = BSC.unpack $ BS.concat
                   [ "public", rqContextPath request, rqPathInfo request, ".html" ]
        when (not $ isRelative path) pass
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
  do directory' <- getLibraryAndVersion
     case directory' of
       Nothing -> error404 "Invalid library name or version number."
       Just directory -> do
         exists <- liftIO $ doesDirectoryExist directory
         if exists
         then error404 "That version has already been registered."
         else handleFileUploads "/tmp" defaultUploadPolicy perPartPolicy (handler directory)
  where
    perPartPolicy partInfo
        | okayPart partInfo = allowWithMaximumSize $ 2^(19::Int)
        | otherwise = disallow

    okayPart part =
        partFieldName part == "docs"
        && partContentType part == "application/json"

    handler directory parts =
        case parts of
          [(info, Right tempDocs)] | okayPart info ->
              do let permanentDocs = directory </> Utils.json
                 result <- liftIO $ do
                             createDirectoryIfMissing True directory
                             BS.writeFile permanentDocs =<< BS.readFile tempDocs
                             runErrorT $ Docs.generate directory
                 case result of
                   Right _ -> writeBS "Registered successfully!"
                   Left err ->
                       do writeBS $ BSC.pack err
                          httpError 500 "Internal Server Error"
              
          [(info, Left err)] ->
              do writeText $ policyViolationExceptionReason err
                 error404 ""

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
  do directory' <- getLibraryAndVersion
     case directory' of
       Nothing -> error404 "Invalid library name or version number."
       Just directory -> do
         exists <- liftIO $ doesDirectoryExist directory
         if exists then serveFile (directory </> Utils.json)
                   else error404 "That library and version is not registered."

getLibraryAndVersion :: Snap (Maybe FilePath)
getLibraryAndVersion =
  do lib <- getParam "library"
     ver <- getParam "version"
     return $ Utils.libraryVersion <$> (N.fromString . BSC.unpack =<< lib)
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
