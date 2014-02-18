{-# OPTIONS_GHC -W    #-}
module Get.Options ( parse
                   , Command(..)
                   )
       where

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
    | Version
    deriving (Show, Eq)

parse :: IO Command
parse = customExecParser prefs parser
  where prefs = Opt.prefs $ mempty <> showHelpOnError

parser :: ParserInfo Command
parser = info (helper <*> commands)
              ( fullDesc
               <> header top
               <> progDesc "install, update, and publish elm libraries"
               <> footer moreHelp
              )
  where top = unwords [ "elm-get"
                      , showVersion This.version ++ ":"
                      , " The Elm Package Manager "
                      , "(c) Evan Czaplicki 2013"]
        moreHelp = unlines
          ["To learn more about a command called COMMAND, just do"
          , "  elm-get COMMAND --help"
          ]

commands :: Parser Command
commands = hsubparser $
     command "install" installOpts
  <> command "update"  updateOpts
  <> command "publish" publishOpts
  <> command "version" versionOpts

installOpts :: ParserInfo Command
installOpts = info
  (Install <$> optional library)
  ( fullDesc
  <> progDesc "Install libraries in the local project."
  <> footer   examples
  )
  where library =
          Library' <$> (argument str (metavar "LIBRARY"
                                      <> help "Library to install"))
                   <*> (optional $
                        argument str (metavar "VERSION"
                                      <> help "Specific version of a project to install"))
        examples = unlines
          [ "Examples:"
          , "  elm-get install                # install everything needed by elm_dependencies.json"
          , "  elm-get install tom/Array      # install a specific github repo"
          , "  elm-get install tom/Array 1.2  # install a specific version tag github repo" ]

updateOpts :: ParserInfo Command
updateOpts = info
  (Update <$> many (argument str (metavar "LIBRARY" <> help "Library to update")))
  ( fullDesc
  <> progDesc "Check for updates to any local libraries, ask to upgrade."
  <> footer   examples
  )
  where examples = unlines
          [ "Examples:"
          , "  elm-get update             # check for updates to local libraries"
          , "  elm-get update tom/Array   # update from a specific github repo" ]

publishOpts :: ParserInfo Command
publishOpts = info
              (pure Publish)
              (fullDesc <> progDesc "Publish project to the central repository")

versionOpts :: ParserInfo Command
versionOpts = info
              (pure Version)
              (fullDesc <> progDesc "Display the project version")
