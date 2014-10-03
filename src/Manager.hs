module Manager where

import Control.Monad.Error
import Control.Monad.Reader
import qualified System.Directory as Dir
import System.FilePath ((</>))


data Command
    = Install (Maybe (String, Maybe String))
    | Publish
    | PrintVersion


type Manager =
    ErrorT String (ReaderT Environment IO)


run :: Environment -> Manager a -> IO (Either String a)
run environment manager =
    runReaderT (runErrorT manager) environment


data Environment = Environment
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
        let dir = root </> "package"
        Dir.createDirectoryIfMissing True dir
        return dir
