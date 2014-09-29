module CommandLine.Options (parse) where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Manager
import qualified Paths_elm_package as This
import qualified Elm.Package.Paths as Path


parse :: IO Manager.Command
parse =
    Opt.customExecParser preferences parser


preferences :: Opt.ParserPrefs
preferences =
    Opt.prefs (mempty <> Opt.showHelpOnError)


parser :: Opt.ParserInfo Manager.Command
parser =
    Opt.info (Opt.helper <*> commands) infoModifier


-- GENERAL HELP

infoModifier :: Opt.InfoMod Manager.Command
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
        ++ ", (c) Evan Czaplicki 2013-2014\n"

    moreHelp =
        linesToDoc
        [ "To learn more about a particular command run:"
        , "    elm-package COMMAND --help"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lines =
    PP.vcat (map PP.text lines)


-- COMMANDS

commands :: Opt.Parser Manager.Command
commands =
    Opt.hsubparser $
        mconcat
        [ Opt.command "install" installInfo
        , Opt.command "publish" publishInfo
        ]


publishInfo :: Opt.ParserInfo Manager.Command
publishInfo =
    Opt.info (pure Manager.Publish) $
        mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Publish your project to the central repository"
        ]


installInfo :: Opt.ParserInfo Manager.Command
installInfo =
    Opt.info (Manager.Install <$> Opt.optional package) infoModifier
  where
    infoModifier =
        mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Install libraries in the local project."
        , Opt.footerDoc (Just examples)
        ]

    examples =
        linesToDoc
        [ "Examples:"
        , "  elm-package install                      # everything needed by " ++ Path.description
        , "  elm-package install evancz/elm-html      # any version"
        , "  elm-package install evancz/elm-html 1.2  # specific version"
        ]


package :: Opt.Parser (String, Maybe String)
package =
    (,) <$> library <*> Opt.optional version
  where
    library =
        Opt.argument Opt.str $
            mconcat
            [ Opt.metavar "LIBRARY"
            , Opt.help "A specific library (e.g. evancz/automaton)"
            ]

    version =
        Opt.argument Opt.str $
            mconcat
            [ Opt.metavar "VERSION"
            , Opt.help "Specific version of a project to install"
            ]
