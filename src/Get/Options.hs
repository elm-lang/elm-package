{-# OPTIONS_GHC -W #-}
module Get.Options ( parse, Command(..) ) where

import Control.Applicative
import Data.Monoid
import Data.Version (showVersion)
import Options.Applicative as Opt

import Get.Library
import qualified Paths_elm_get as This

data Command
    = Install (Maybe RawLibrary)
    | Update  { libs :: [String] }
    | Publish
    deriving (Show, Eq)

parse :: IO Command
parse = customExecParser prefs parser
  where
    prefs = Opt.prefs $ mempty <> showHelpOnError

parser :: ParserInfo Command
parser = info (helper <*> commands) infoMod
  where
    infoMod = mconcat
        [ fullDesc
        , header top
        , progDesc "install and publish elm libraries"
        , footer moreHelp
        ]

    top = unwords
        [ "elm-get", showVersion This.version ++ ":"
        , "The Elm Package Manager (c) Evan Czaplicki 2013-2014\n"
        ]

    moreHelp = unlines
        [ "To learn more about a command called COMMAND, just do:"
        , "  elm-get COMMAND --help"
        ]

commands :: Parser Command
commands =
    hsubparser $ mconcat
    [ command "install" installOpts
    , command "publish" publishOpts
--    , command "update"  updateOpts -- TODO: implement update
    ]

installOpts :: ParserInfo Command
installOpts = info (Install <$> optional library) infoMod
  where
    infoMod = mconcat
        [ fullDesc
        , progDesc "Install libraries in the local project."
        , footer examples
        ]

    examples = unlines
        [ "Examples:"
        , "  elm-get install                # install everything needed by elm_dependencies.json"
        , "  elm-get install tom/Array      # install a specific github repo"
        , "  elm-get install tom/Array 1.2  # install a specific version tag github repo"
        ]

library :: Parser RawLibrary
library = Library <$> lib <*> optional ver
  where
    lib = argument str (metavar "LIBRARY"
                        <> help "A specific library (e.g. evancz/automaton)")

    ver = argument str (metavar "VERSION"
                        <> help "Specific version of a project to install")

-- | TODO: restore when update is actually implemented
-- updateOpts :: ParserInfo Command
-- updateOpts = info
--   (Update <$> many (argument str (metavar "LIBRARY" <> help "Library to update")))
--   ( fullDesc
--   <> progDesc "Check for updates to any local libraries, ask to upgrade."
--   <> footer   examples
--   )
--   where examples = unlines
--           [ "Examples:"
--           , "  elm-get update             # check for updates to local libraries"
--           , "  elm-get update tom/Array   # update from a specific github repo" ]

publishOpts :: ParserInfo Command
publishOpts =
    info (pure Publish)
         (fullDesc <> progDesc "Publish project to the central repository")
