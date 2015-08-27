{-# LANGUAGE FlexibleContexts #-}
module Install.Fetch where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Codec.Archive.Zip as Zip
import qualified Data.List as List
import qualified Network.HTTP.Client as Client
import System.Directory (doesDirectoryExist, getDirectoryContents, renameDirectory)
import System.FilePath ((</>))

import qualified Elm.Package as Package
import qualified CommandLine.Helpers as Cmd
import qualified Utils.Http as Http


package :: (MonadIO m, MonadError String m) => Package.Name -> Package.Version -> m ()
package name@(Package.Name user _) version =
  ifNotExists name version $ do
      Http.send zipball extract
      files <- liftIO $ getDirectoryContents "."
      case List.find (List.isPrefixOf user) files of
        Nothing ->
            throwError "Could not download source code successfully."
        Just dir ->
            liftIO $ renameDirectory dir (Package.versionToString version)
  where
    zipball =
        "http://github.com/" ++ Package.toUrl name ++ "/zipball/" ++ Package.versionToString version ++ "/"


ifNotExists :: (MonadIO m, MonadError String m) => Package.Name -> Package.Version -> m () -> m ()
ifNotExists name version command =
  do  let directory = Package.toFilePath name
      exists <- liftIO $ doesDirectoryExist (directory </> Package.versionToString version)
      if exists
        then return ()
        else Cmd.inDir directory command


extract :: Client.Request -> Client.Manager -> IO ()
extract request manager =
    do  response <- Client.httpLbs request manager
        let archive = Zip.toArchive (Client.responseBody response)
        Zip.extractFilesFromArchive [] archive
