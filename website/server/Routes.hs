{-# LANGUAGE OverloadedStrings,DoAndIfThenElse #-}
module Routes where

import Control.Applicative
import Control.Monad.Error
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Snap.Core
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.Directory
import System.FilePath

import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Paths as EPath
import qualified Elm.Internal.Version as V
import qualified Generate.Docs as Docs
import qualified Generate.Listing as Listing
import qualified Utils.Http as Http
import qualified Utils.Paths as Path
import qualified NativeWhitelist

catalog :: Snap ()
catalog =
    ifTop (serveFile "public/Catalog.html")
    <|> route [ (":name/:version", serveLibrary) ]
  where
    serveLibrary :: Snap ()
    serveLibrary = do
      request <- getRequest
      redirectIfLatest request
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

    redirectIfLatest :: Request -> Snap ()
    redirectIfLatest request =
      case (,) <$> rqParam "name" request <*> rqParam "version" request of
        Just ([name], ["latest"]) ->
            let namePath = "catalog" </> BSC.unpack name in
            do rawVersions <- liftIO (getDirectoryContents ("public" </> namePath))
               case Maybe.catMaybes (map V.fromString rawVersions) of
                 vs@(_:_) -> redirect path'
                     where
                       path' = BSC.concat [ "/", BSC.pack project, "/", rqPathInfo request ]
                       project = namePath </> show version
                       version = last (List.sort vs)

                 _ -> return ()

        _ -> return ()


register :: Snap ()
register =
  do nameAndVersion <- getNameAndVersion
     withErrorT (verifyVersion nameAndVersion) $ \(name,version) -> do
       let directory = Path.libraryVersion name version
       uploadFiles directory
       withErrorT (generateDocs directory) return

  where
    withErrorT :: ErrorT String IO a -> (a -> Snap ()) -> Snap ()
    withErrorT errT callback = do
      either <- liftIO $ runErrorT errT
      case either of
        Left err -> error404' err
        Right result -> callback result

    generateDocs :: FilePath -> ErrorT String IO ()
    generateDocs directory = do
      (docs,deps) <- Docs.readDocsAndDeps directory
      NativeWhitelist.verify deps
      Docs.generate docs deps directory

verifyVersion :: Maybe (N.Name, V.Version) -> ErrorT String IO (N.Name, V.Version)
verifyVersion nameAndVersion =
  do let err = throwError "Invalid library name or version number."
     (name,version) <- maybe err return nameAndVersion

     listings <- liftIO $ Listing.readListings
     let maybeListing = List.find (\lstng -> name == Listing.name lstng) listings
         versions = maybe [] Listing.versions maybeListing
     when (version `elem` versions) $
          throwError ("Version " ++ show version ++ " has already been registered.")

     (Http.Tags vs) <- Http.githubTags name
     let v = show version
     when (v `notElem` vs) $
          throwError ("The tag " ++ v ++ " has not been pushed to GitHub.")

     return (name,version)

uploadFiles :: FilePath -> Snap ()
uploadFiles directory =
  do liftIO $ createDirectoryIfMissing True directory
     handleFileUploads "/tmp" defaultUploadPolicy perPartPolicy (handler directory)
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
            liftIO $ do
              BS.readFile temp1 >>= BS.writeFile (dir </> Path.json)
              BS.readFile temp2 >>= BS.writeFile (dir </> EPath.dependencyFile)
        | okayPart "docs" info2 && okayPart "deps" info1 =
            liftIO $ do
              BS.readFile temp2 >>= BS.writeFile (dir </> Path.json)
              BS.readFile temp1 >>= BS.writeFile (dir </> EPath.dependencyFile)
    handler _ parts =
        do mapM (writeError . snd) parts
           error404' msg

    writeError = either (writeText . policyViolationExceptionReason) (const (return ()))
    msg = "Files " ++ Path.json ++ " and " ++ EPath.dependencyFile ++ " were not uploaded."

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
