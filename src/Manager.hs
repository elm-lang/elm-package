module Manager where

import Control.Monad.Except
import Control.Monad.Reader
import qualified System.Directory as Dir
import System.FilePath ((</>))


type Manager =
    ExceptT String (ReaderT Environment IO)


run :: Environment -> Manager a -> IO (Either String a)
run environment manager =
    runReaderT (runExceptT manager) environment


data Environment = Environment
    { catalog :: String
    , cacheDirectory :: FilePath
    }


defaultEnvironment :: IO Environment
defaultEnvironment =
    do  cacheDirectory <- getCacheDirectory
        return (Environment "http://162.243.138.108:1234" cacheDirectory)


getCacheDirectory :: IO FilePath
getCacheDirectory =
    do  root <- Dir.getAppUserDataDirectory "elm"
        let dir = root </> "package"
        Dir.createDirectoryIfMissing True dir
        return dir
