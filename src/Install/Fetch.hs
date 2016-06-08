module Install.Fetch (everything) where

import Control.Concurrent.ParallelIO.Local (withPool, parallel)
import Control.Monad.Except (liftIO, throwError)
import qualified Codec.Archive.Zip as Zip
import qualified Data.List as List
import GHC.IO.Handle (hIsTerminalDevice)
import qualified Network.HTTP.Client as Client
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist
  , getDirectoryContents, renameDirectory
  )
import System.FilePath ((</>))
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), (<+>), bold, displayIO, green, plain, red, renderPretty, text )

import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import qualified CommandLine.Helpers as Cmd
import qualified Manager
import qualified Reporting.Error as Error
import qualified Utils.Http as Http



everything :: [(Pkg.Name, Pkg.Version)] -> Manager.Manager ()
everything packages =
  Cmd.inDir Path.packagesDirectory $
    do  eithers <- liftIO $ do
          startMessage (length packages)
          isTerminal <- hIsTerminalDevice stdout
          withPool 4 $ \pool ->
            parallel pool (map (prettyFetch isTerminal) packages)

        case sequence eithers of
          Right _ ->
            liftIO $ putStrLn ""

          Left err ->
            throwError err


prettyFetch :: Bool -> (Pkg.Name, Pkg.Version) -> IO (Either Error.Error ())
prettyFetch isTerminal (name, version) =
  do  result <- Manager.run $ fetch name version
      let doc = toDoc result name version
      displayIO stdout $ renderPretty 1 80 $
        if isTerminal then doc else plain doc
      return result



startMessage :: Int -> IO ()
startMessage n =
  if n > 0 then
    putStrLn "Starting downloads...\n"

  else
    return ()


toDoc :: Either a b -> Pkg.Name -> Pkg.Version -> Doc
toDoc result name version =
  let
    nameDoc =
      text $ Pkg.toString name

    versionDoc =
      text $ Pkg.versionToString version

    bullet =
      case result of
        Right _ ->
          green (text "»")

        Left _ ->
          red (text "×")
  in
    text "  " <> bold bullet <+> nameDoc <+> versionDoc <> text "\n"



-- FETCH A PACKAGE


fetch :: Pkg.Name -> Pkg.Version -> Manager.Manager ()
fetch name@(Pkg.Name user project) version =
  ifNotExists name version $
    do  Http.send (toZipballUrl name version) extract
        files <- liftIO $ getDirectoryContents "."
        case List.find (List.isPrefixOf (user ++ "-" ++ project)) files of
          Nothing ->
            throwError $ Error.ZipDownloadFailed name version

          Just dir ->
            liftIO $ do
              let home = Pkg.toFilePath name
              createDirectoryIfMissing True home
              renameDirectory dir (home </> Pkg.versionToString version)


toZipballUrl :: Pkg.Name -> Pkg.Version -> String
toZipballUrl name version =
  "https://github.com/" ++ Pkg.toUrl name
  ++ "/zipball/" ++ Pkg.versionToString version ++ "/"


ifNotExists :: Pkg.Name -> Pkg.Version -> Manager.Manager () -> Manager.Manager ()
ifNotExists name version task =
  do  let dir = Pkg.toFilePath name </> Pkg.versionToString version
      exists <- liftIO $ doesDirectoryExist dir
      if exists then return () else task


extract :: Client.Request -> Client.Manager -> IO ()
extract request manager =
  do  response <- Client.httpLbs request manager
      let archive = Zip.toArchive (Client.responseBody response)
      Zip.extractFilesFromArchive [] archive
