module Manager
  ( Manager
  , run
  , Environment(..)
  )
  where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Elm.Compiler as Elm
import qualified Elm.Package as Pkg
import qualified Network
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Reporting.Error as Error


type Manager =
  ExceptT Error.Error (ReaderT Environment IO)


run :: Manager a -> IO (Either Error.Error a)
run manager =
  Network.withSocketsDo $
    do  cacheDirectory <- getCacheDirectory
        httpManager <- Http.newManager Http.tlsManagerSettings
        let env = Environment "http://package.elm-lang.org:8018" cacheDirectory httpManager
        runReaderT (runExceptT manager) env


data Environment =
  Environment
    { catalog :: String
    , cacheDirectory :: FilePath
    , httpManager :: Http.Manager
    }


getCacheDirectory :: IO FilePath
getCacheDirectory =
  do  root <- Dir.getAppUserDataDirectory "elm"
      let dir = root </> Pkg.versionToString Elm.version </> "package"
      Dir.createDirectoryIfMissing True dir
      return dir
