module Manager where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Elm.Compiler as Elm
import qualified Elm.Package as Pkg
import qualified System.Directory as Dir
import System.FilePath ((</>))


type Manager =
  ExceptT String (ReaderT Environment IO)


run :: Environment -> Manager a -> IO (Either String a)
run environment manager =
  runReaderT (runExceptT manager) environment


data Environment =
  Environment
    { catalog :: String
    , cacheDirectory :: FilePath
    }


defaultEnvironment :: IO Environment
defaultEnvironment =
  do  cacheDirectory <- getCacheDirectory
      return (Environment "http://package.elm-lang.org" cacheDirectory)


getCacheDirectory :: IO FilePath
getCacheDirectory =
  do  root <- Dir.getAppUserDataDirectory "elm"
      let dir = root </> Pkg.versionToString Elm.version </> "package"
      Dir.createDirectoryIfMissing True dir
      return dir
