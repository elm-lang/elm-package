{-# OPTIONS_GHC -W #-}
module Get.Options ( parse, Command(..) ) where

import Control.Applicative
import Data.Monoid
import Data.Version (showVersion)
import Options.Applicative as Opt
import Text.PrettyPrint.ANSI.Leijen (text, vcat, Doc)

import Get.Library
import qualified Paths_elm_get as This

data Command
    = Install (Maybe RawLibrary)
    | Update  { libs :: [String] }
    | Publish
    | Init
    deriving (Show, Eq)

parse :: IO Command
parse = customExecParser prefs parser
  where
    prefs = Opt.prefs $ mempty <> showHelpOnError

linesDoc :: [String] -> Doc
linesDoc = vcat . map text

parser :: ParserInfo Command
parser = info (helper <*> commands) infoMod
  where
    infoMod = mconcat
        [ fullDesc
        , header top
        , progDesc "install and publish elm libraries"
        , footerDoc (Just moreHelp)
        ]

    top = unwords
        [ "elm-get", showVersion This.version ++ ":"
        , "The Elm Package Manager (c) Evan Czaplicki 2013-2014\n"
        ]

    moreHelp = linesDoc
        [ "To learn more about a command called COMMAND, just do:"
        , "  elm-get COMMAND --help"
        ]

commands :: Parser Command
commands =
    hsubparser $ mconcat
    [ command "install" installOpts
    , command "publish" publishOpts
--    , command "update"  updateOpts -- TODO: implement update
    , command "init" initOpts
    ]

installOpts :: ParserInfo Command
installOpts = info (Install <$> optional library) infoMod
  where
    infoMod = mconcat
        [ fullDesc
        , progDesc "Install libraries in the local project."
        , footerDoc (Just examples)
        ]

    examples = linesDoc
        [ "Examples:"
        , "  elm-get install                # install everything needed by elm_dependencies.json"
        , "  elm-get install tom/Array      # install a specific github repo"
        , "  elm-get install tom/Array 1.2  # install a specific version tag github repo"
        ]

initOpts :: ParserInfo Command
initOpts = info (pure Init) infoMod
  where
    infoMod = mconcat
        [ fullDesc
        , progDesc "Initialize elm_dependencies.json in working directory"
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
--   <> footerDoc   examples
--   )
--   where examples = linesDoc
--           [ "Examples:"
--           , "  elm-get update             # check for updates to local libraries"
--           , "  elm-get update tom/Array   # update from a specific github repo" ]

publishOpts :: ParserInfo Command
publishOpts =
    info (pure Publish)
         (fullDesc <> progDesc "Publish project to the central repository")
