module Install.Fetch (everything) where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
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
  ( Doc, (<>), (<+>), displayIO, green, plain, red, renderPretty, text )

import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import qualified CommandLine.Helpers as Cmd
import qualified Manager
import qualified Reporting.Error as Error
import qualified Utils.Http as Http



-- PARALLEL FETCHING


everything :: [(Pkg.Name, Pkg.Version)] -> Manager.Manager ()
everything packages =
  Cmd.inDir Path.packagesDirectory $
    do  eithers <- liftIO $ do
          startMessage (length packages)
          isTerminal <- hIsTerminalDevice stdout
          resultChan <- Chan.newChan
          forkIO (printLoop isTerminal resultChan)
          withPool 4 $ \pool ->
            parallel pool (map (prettyFetch resultChan) packages)

        case sequence eithers of
          Right _ ->
            liftIO $ putStrLn ""

          Left err ->
            throwError err


data Result =
  Result
    { _name :: Pkg.Name
    , _vsn :: Pkg.Version
    , _either :: Either Error.Error ()
    }


printLoop :: Bool -> Chan.Chan Result -> IO ()
printLoop isTerminal resultChan =
  do  result <- Chan.readChan resultChan
      let doc = toDoc result
      displayIO stdout $ renderPretty 1 80 $
        if isTerminal then doc else plain doc
      printLoop isTerminal resultChan


prettyFetch :: Chan.Chan Result -> (Pkg.Name, Pkg.Version) -> IO (Either Error.Error ())
prettyFetch printChan (name, version) =
  do  either <- Manager.run $ fetch name version
      Chan.writeChan printChan (Result name version either)
      return either


startMessage :: Int -> IO ()
startMessage n =
  if n > 0 then
    putStrLn "Starting downloads..."

  else
    return ()


toDoc :: Result -> Doc
toDoc (Result name version either) =
  let
    nameDoc =
      text $ Pkg.toString name

    versionDoc =
      text $ Pkg.versionToString version

    bullet =
      case either of
        Right _ ->
          green (text "●")

        Left _ ->
          red (text "✗")
  in
    text "  " <> bullet <+> nameDoc <+> versionDoc <> text "\n"



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
