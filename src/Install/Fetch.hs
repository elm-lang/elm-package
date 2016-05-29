module Install.Fetch (package) where

import Control.Monad.Except (liftIO, throwError)
import qualified Codec.Archive.Zip as Zip
import qualified Data.List as List
import qualified Network.HTTP.Client as Client
import System.Directory (doesDirectoryExist, getDirectoryContents, renameDirectory)
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified CommandLine.Helpers as Cmd
import qualified Manager
import qualified Reporting.Error as Error
import qualified Utils.Http as Http


package :: Pkg.Name -> Pkg.Version -> Manager.Manager ()
package name@(Pkg.Name user _) version =
  ifNotExists name version $
    do  Http.send (toZipballUrl name version) extract
        files <- liftIO $ getDirectoryContents "."
        case List.find (List.isPrefixOf user) files of
          Nothing ->
            throwError $ Error.ZipDownloadFailed name version

          Just dir ->
            liftIO $ renameDirectory dir (Pkg.versionToString version)


toZipballUrl :: Pkg.Name -> Pkg.Version -> String
toZipballUrl name version =
  "https://github.com/" ++ Pkg.toUrl name
  ++ "/zipball/" ++ Pkg.versionToString version ++ "/"



ifNotExists :: Pkg.Name -> Pkg.Version -> Manager.Manager () -> Manager.Manager ()
ifNotExists name version command =
  do  let directory = Pkg.toFilePath name
      exists <- liftIO $ doesDirectoryExist (directory </> Pkg.versionToString version)
      if exists
        then return ()
        else Cmd.inDir directory command


extract :: Client.Request -> Client.Manager -> IO ()
extract request manager =
  do  response <- Client.httpLbs request manager
      let archive = Zip.toArchive (Client.responseBody response)
      Zip.extractFilesFromArchive [] archive
