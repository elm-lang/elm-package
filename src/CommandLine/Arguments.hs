module CommandLine.Arguments (parse) where

import Control.Applicative (pure, optional, (<$>), (<*>), (<|>))
import Control.Monad.Error.Class (throwError)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Bump
import qualified Diff
import qualified Install
import qualified Manager
import qualified Publish
import qualified Paths_elm_package as This
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Package
import qualified Elm.Package.Paths as Path


parse :: IO (Manager.Manager ())
parse =
    Opt.customExecParser preferences parser


preferences :: Opt.ParserPrefs
preferences =
    Opt.prefs (mempty <> Opt.showHelpOnError)


parser :: Opt.ParserInfo (Manager.Manager ())
parser =
    Opt.info (Opt.helper <*> commands) infoModifier


-- GENERAL HELP

infoModifier :: Opt.InfoMod (Manager.Manager ())
infoModifier =
    mconcat
        [ Opt.fullDesc
        , Opt.header top
        , Opt.progDesc "install and publish elm libraries"
        , Opt.footerDoc (Just moreHelp)
        ]
  where
    top =
        "Elm Package Manager " ++ showVersion This.version
        ++ " (Elm Platform " ++ (Package.versionToString Compiler.version) ++ ")\n"

    moreHelp =
        linesToDoc
        [ "To learn more about a particular command run:"
        , "    elm-package COMMAND --help"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lines =
    PP.vcat (map PP.text lines)


-- COMMANDS

commands :: Opt.Parser (Manager.Manager ())
commands =
    Opt.hsubparser commandOptions
  where
    version =
        Opt.flag' (error "temporarily out of order")
            (Opt.long "version" <> Opt.short 'v' <> Opt.hidden)

    commandOptions =
        mconcat
        [ Opt.command "install" installInfo
        , Opt.command "publish" publishInfo
        , Opt.command "bump" bumpInfo
        , Opt.command "diff" diffInfo
        ]


-- BUMP

bumpInfo :: Opt.ParserInfo (Manager.Manager ())
bumpInfo =
    Opt.info (pure Bump.bump) $
        mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Bump version numbers based on API changes"
        ]


-- DIFF

diffInfo :: Opt.ParserInfo (Manager.Manager ())
diffInfo =
    Opt.info (Diff.diff <$> range) $
        mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Get differences between two APIs"
        ]
  where
    range =
        (Diff.Between <$> package <*> version <*> version)
        <|> (Diff.Since <$> version)
        <|> (pure Diff.LatestVsActual)


-- PUBLISH

publishInfo :: Opt.ParserInfo (Manager.Manager ())
publishInfo =
    Opt.info (pure Publish.publish) $
        mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Publish your package to the central catalog"
        ]


-- INSTALL

installInfo :: Opt.ParserInfo (Manager.Manager ())
installInfo =
    Opt.info args infoModifier
  where
    args =
        installWith <$> optional package <*> optional version <*> yes

    installWith maybeName maybeVersion autoYes =
        case (maybeName, maybeVersion) of
          (Nothing, Nothing) ->
              Install.install autoYes Install.Everything

          (Just name, Nothing) ->
              Install.install autoYes (Install.Latest name)

          (Just name, Just version) ->
              Install.install autoYes (Install.Exactly name version)

          (Nothing, Just version) ->
              throwError $
                "You specified a version number, but not a package!\nVersion "
                ++ Package.versionToString version ++ " of what?"

    infoModifier =
        mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Install packages to use locally"
        , Opt.footerDoc (Just examples)
        ]

    examples =
        linesToDoc
        [ "Examples:"
        , "  elm-package install                        # everything needed by " ++ Path.description
        , "  elm-package install evancz/elm-html        # any version"
        , "  elm-package install evancz/elm-html 1.2.0  # specific version"
        ]


-- ARGUMENT PARSERS

package :: Opt.Parser Package.Name
package =
    Opt.argument (argReader "PACKAGE" Package.fromString) $
        mconcat
        [ Opt.metavar "PACKAGE"
        , Opt.help "A specific package name (e.g. evancz/automaton)"
        ]


version :: Opt.Parser Package.Version
version =
    Opt.argument (argReader "VERSION" Package.versionFromString) $
        mconcat
        [ Opt.metavar "VERSION"
        , Opt.help "Specific version of a package (e.g. 1.2.0)"
        ]


yes :: Opt.Parser Bool
yes =
    Opt.switch $
        mconcat
        [ Opt.long "yes"
        , Opt.short 'y'
        , Opt.help "Reply 'yes' to all automated prompts."
        ]


argReader :: String -> (String -> Maybe a) -> Opt.ReadM a
argReader argType fromString =
  let reader arg =
          case fromString arg of
            Just a ->
                Right a

            Nothing ->
                Left ("Uh oh, argument \"" ++ arg ++ "\" is not a valid " ++ argType)
  in
      Opt.eitherReader reader
