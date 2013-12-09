{-# LANGUAGE OverloadedStrings #-}
module Registry.Routes where

import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Control.Applicative
import Control.Monad.Error

import Snap.Core
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.Directory
import System.FilePath

import qualified Utils.Paths as Path
import qualified Utils.Model.Name as N
import qualified Utils.Model.Version as V
import qualified Registry.Generate.Docs as Docs

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

register :: Snap ()
register =
  do nameAndVersion <- getNameAndVersion
     case nameAndVersion of
       Nothing -> error404 "Invalid library name or version number."
       Just (name,version) -> do
         let directory = Path.libraryVersion name version
         exists <- liftIO $ doesDirectoryExist directory
         if exists
           then error404 "That version has already been registered."
           else do
             liftIO $ createDirectoryIfMissing True directory
             handleFileUploads "/tmp" defaultUploadPolicy perPartPolicy (handler directory)
             result <- liftIO $ runErrorT $ Docs.generate directory
             case result of
               Right () -> writeBS "Registered successfully!"
               Left err ->
                   do liftIO $ removeDirectoryRecursive directory
                      writeBS $ BSC.pack err
                      httpError 500 "Internal Server Error"
  where
    perPartPolicy info
        | okayPart "docs" info || okayPart "deps" info = allowWithMaximumSize $ 2^(19::Int)
        | otherwise = disallow

    okayPart field part =
        partFieldName part == field
        && partContentType part == "application/json"

    handler :: FilePath -> [(PartInfo, Either PolicyViolationException FilePath)] -> Snap ()
    handler dir [(info1, Right temp1), (info2, Right temp2)] 
        | okayPart "docs" info1 && okayPart "deps" info2 =
            liftIO $ do BS.readFile temp1 >>= BS.writeFile (dir </> Path.json)
                        BS.readFile temp2 >>= BS.writeFile (dir </> Path.depsFile)
        | okayPart "docs" info2 && okayPart "deps" info1 =
            liftIO $ do BS.readFile temp2 >>= BS.writeFile (dir </> Path.json)
                        BS.readFile temp1 >>= BS.writeFile (dir </> Path.depsFile)
    handler dir parts =
        do mapM (writeError . snd) parts
           error404' msg

    writeError = either (writeText . policyViolationExceptionReason) (const (return ()))
    msg = "Files " ++ Path.json ++ " and " ++ Path.depsFile ++ " were not uploaded."

versions :: Snap ()
versions = do
  library <- getParam "library"
  case N.fromString . BSC.unpack =<< library of
    Nothing -> error404 "The request arguments are not well-formed."
    Just name ->
        do let path = Path.library name
           exists <- liftIO $ doesDirectoryExist path
           versions <- case exists of
                         False -> return Nothing
                         True  -> do contents <- liftIO $ getDirectoryContents path
                                     return $ Just $ Maybe.mapMaybe V.fromString contents
           writeLBS (Binary.encode versions)

metadata :: Snap ()
metadata =
  do nameAndVersion <- getNameAndVersion
     case uncurry Path.libraryVersion <$> nameAndVersion of
       Nothing -> error404 "Invalid library name or version number."
       Just directory -> do
         exists <- liftIO $ doesDirectoryExist directory
         if exists then serveFile (directory </> Path.json)
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
