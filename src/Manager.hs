module Manager where

import Control.Monad.Error
import Control.Monad.Reader
import qualified System.Directory as Dir
import System.FilePath ((</>))


type Manager =
    ErrorT String (ReaderT Environment IO)


data Environment = Environment
    { catalog :: String
    , cacheDirectory :: FilePath
    }


run :: Environment -> Manager a -> IO (Either String a)
run environment manager =
    runReaderT (runErrorT manager) environment


data Command
    = Install (Maybe (String, Maybe String))
    | Publish


getCacheDirectory :: IO FilePath
getCacheDirectory =
    do  root <- Dir.getAppUserDataDirectory "elm"
        let dir = root </> "package"
        Dir.createDirectoryIfMissing True dir
        return dir
