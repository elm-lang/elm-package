{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where
 
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import qualified Paths_elm_get as This
import Data.Version

data Commands
    = Install { libs :: [String] }
    | Update { libs_ :: [String] }
    | Publish { repo :: Maybe String }
      deriving (Data, Typeable, Show, Eq)

commands =
    [ Install { libs = [] &= args &= typ "LIBRARY" }
      &= help "Install libraries in the local project."
    , Update { libs_ = [] &= args &= typ "LIBRARY" }
      &= help "Check for updates to any local libraries, ask to upgrade."
    , Publish { repo = Nothing &= argPos 0 &= typ "REPO" }
      &= help "Publish project to the central repository."
    ]

myModes :: Mode (CmdArgs Commands)
myModes = cmdArgsMode $ modes commands
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", (c) Evan Czaplicki 2013")
    &= help "install, update, and publish elm libraries"
    &= helpArg [explicit, name "help", name "h"]
    &= program "elm-get"
 
_PROGRAM_INFO = "elm-get version " ++ showVersion This.version

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
    print opts
