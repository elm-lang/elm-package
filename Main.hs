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
      &= details [ "  Examples:"
                 , "    elm-get install            # install everything needed by build.json"
                 , "    elm-get install Vector2D   # install library from the central repo"
                 , "    elm-get install sam/Array  # install a specific github repo" ]
    , Update { libs_ = [] &= args &= typ "LIBRARY" }
      &= help "Check for updates to any local libraries, ask to upgrade."
      &= details [ "  Examples:"
                 , "    elm-get update             # check for updates to local libraries"
                 , "    elm-get update Vector2D    # update library from the central repo"
                 , "    elm-get update sam/Array   # update a specific github repo" ]
    , Publish { repo = Nothing &= args &= typ "REPO" }
      &= help "Publish project to the central repository."
      &= details []
    ]

myModes :: Mode (CmdArgs Commands)
myModes = cmdArgsMode $ modes commands
    &= versionArg [explicit, name "version", name "v", summary info]
    &= summary (info ++ ", (c) Evan Czaplicki 2013")
    &= help "install, update, and publish elm libraries"
    &= helpArg [explicit, name "help", name "h"]
    &= program "elm-get"
 
info = "elm-get " ++ showVersion This.version

main :: IO ()
main = do
  args <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
  print opts
