{-# OPTIONS_GHC -Wall #-}
module Reporting.Error
  ( Error(..)
  , Hint(..)
  , toString
  , toStderr
  , nearbyNames
  )
  where

import Data.Function (on)
import qualified Data.List as List
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Paths as Path
import GHC.IO.Handle (hIsTerminalDevice)
import System.IO (hPutStr, stderr)
import qualified Text.EditDistance as Dist
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<+>), (<>), align, displayS, displayIO, dullred, dullyellow, fillSep
  , hardline, indent, plain, red, renderPretty, text, underline, vcat
  )

import qualified Diff.Magnitude as Diff



-- ALL POSSIBLE ERRORS


data Error
  = BadElmVersion Pkg.Version Bool C.Constraint
  | SystemCallFailed String
  | HttpRequestFailed String String
  | ZipDownloadFailed Pkg.Name Pkg.Version
  | CorruptJson String Pkg.Name Pkg.Version
  | CorruptDescription String
  | CorruptDocumentation String
  | CorruptSolution String
  | CorruptVersionCache Pkg.Name
  | PackageNotFound Pkg.Name [Pkg.Name]
  | AddTrickyConstraint Pkg.Name Pkg.Version C.Constraint

  | ConstraintsHaveNoSolution [Hint]

  | BadInstall Pkg.Version

  | Undiffable
  | VersionInvalid
  | VersionJustChanged
  | BadMetadata [String]
  | MissingTag Pkg.Version

  | AlreadyPublished Pkg.Version
  | Unbumpable Pkg.Version [Pkg.Version]
  | InvalidBump Pkg.Version Pkg.Version
  | BadBump Pkg.Version Pkg.Version Diff.Magnitude Pkg.Version Diff.Magnitude



-- BAD CONSTRAINT HINTS


data Hint
  = EmptyConstraint Pkg.Name C.Constraint
  | IncompatibleConstraint Pkg.Name C.Constraint Pkg.Version
  | IncompatiblePackage Pkg.Name



-- NEARBY NAMES


nearbyNames :: Pkg.Name -> [Pkg.Name] -> [Pkg.Name]
nearbyNames package allPackages =
  let
    name =
      Pkg.toString package

    ratedNames =
      map (\pkg -> (distance name (Pkg.toString pkg), pkg)) allPackages

    sortedNames =
      List.sortBy (compare `on` fst) ratedNames
  in
    map snd $ take 4 sortedNames


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y



-- TO MESSAGE


data Message =
  Message
    { _summary :: String
    , _details :: [Doc]
    }


toMessage :: Error -> Message
toMessage err =
  case err of
    BadElmVersion elmVersion isGreater elmConstraint ->
        Message
          ( "You are using Elm " ++ Pkg.versionToString elmVersion
            ++ ", but this project is saying it needs a version in this range: "
            ++ C.toString elmConstraint
          )
          ( map reflow $
              if isGreater then
                [ "This means this package has not been upgraded for the newer version of Elm yet.\
                  \ Check out the upgrade docs for guidance on how to get things working again:\
                  \ <https://github.com/elm-lang/elm-platform/tree/master/upgrade-docs>"
                ]
              else
                [ "This means the package is written for a newer version of Elm. The best route\
                  \ is to just download the new Elm version! <http://elm-lang.org/install>"
                , "If you cannot upgrade for some reason, you can install different versions at\
                  \ the same time with npm. I switch between versions by changing my PATH to\
                  \ point at certain binaries, but you can do it however you want."
                ]
          )

    SystemCallFailed problem ->
      Message "A system call failed." [ text problem ]

    HttpRequestFailed url message ->
      Message
        ( "The following HTTP request failed. <" ++ url ++ ">"
        )
        [ text message
        ]

    ZipDownloadFailed name version ->
      Message
        ( "Problem when downloading the " ++ Pkg.toString name
          ++ " " ++ Pkg.versionToString version ++ " code."
        )
        []

    CorruptJson path name version ->
      Message
        ( "I just fetched " ++ path ++ " for " ++ Pkg.toString name
          ++ " " ++ Pkg.versionToString version
          ++ ", but I cannot read the contents."
        )
        [ reflow $
            "Maybe it is a very old file, and the file format changed since then?\
            \ Or maybe you are at a hotel or airport where they hijack your HTTP\
            \ requests and redirect you to some log in page?"
        ]

    CorruptDescription problem ->
      Message
        ( "The description in " ++ Path.description ++ " is not valid."
        )
        [ text problem
        ]

    CorruptDocumentation problem ->
      Message
        ( "I was able to produce documentation for your package, but it is not valid.\
          \ My guess is that the elm-package and elm-make on your PATH are not from the\
          \ same version of Elm, but it could be some other similarly crazy thing."
        )
        [ text problem
        ]

    CorruptSolution problem ->
      Message
        ( "Your " ++ Path.solvedDependencies ++ " file is corrupted. Do not modify it\
          \ by hand! You can just delete it and I will recreate a valid one."
        )
        [ text problem
        ]

    CorruptVersionCache name ->
      Message
        ( "Your .elm/packages/ directory may be corrupted. I was led to beleive\
          \ that " ++ Pkg.toString name ++ " existed, but I could not find anything\
          \ when I went to look up the published versions of this package."
        )
        []

    ConstraintsHaveNoSolution hints ->
      Message "I cannot find a set of packages that will work with your constraints." $
        case hints of
          [] ->
            [ reflow $
                "One way to rebuild your constraints is to clear everything out of\
                \ the \"dependencies\" field of " ++ Path.description ++ " and add\
                \ them back one at a time with `elm-package install`."
            , reflow $
                "I hope to automate this in the future, but at least there is\
                \ a way to make progress for now!"
            ]

          _ ->
            [ stack (map hintToBullet hints) ]

    PackageNotFound package suggestions ->
      Message
        ( "Could not find any packages named " ++ Pkg.toString package ++ "."
        )
        [ text $ "Here are some packages that have similar names:"
        , indent 4 $ vcat $ map (text . Pkg.toString) suggestions
        , text $ "Maybe you want one of those?"
        ]

    AddTrickyConstraint name version constraint ->
      Message
        ( "This change is too tricky for me. Your " ++ Path.description
          ++ " already lists the following dependency:"
        )
        [ indent 4 $ text $ showDependency name constraint
        , reflow $
            "So I am not sure how to make that include version "
            ++ Pkg.versionToString version
            ++ " as well. Maybe you want one of the following constraints?"
        , indent 4 $ vcat $ map text $
            [ C.toString (C.expand constraint version)
            , C.toString (C.untilNextMajor version)
            ]
        , reflow $
            "Modify " ++ Path.description ++ " by hand to be exactly what you want."
        ]

    BadInstall version ->
      Message
        ( "You specified a version number, but not a package! Version "
          ++ Pkg.versionToString version ++ " of what?"
        )
        []

    Undiffable ->
      Message "This package has not been published, there is nothing to diff against!" []

    VersionInvalid ->
      Message
        "Cannot publish a package with an invalid version. Use `elm-package bump` to\
        \ figure out what the next version should be, and be sure you commit any\
        \ changes and tag them appropriately."
        []

    VersionJustChanged ->
      Message
        "Cannot publish a package with an invalid version. Be sure you commit any\
        \ necessary changes and tag them appropriately."
        []

    BadMetadata problems ->
      Message
        ( "Some of the fields in " ++ Path.description ++ " have not been filled in yet:"
        )
        [ vcat (map text problems)
        , text $ "Fill these in and try to publish again!"
        ]

    MissingTag version ->
      let
        vsn =
          Pkg.versionToString version
      in
        Message
          ( "Libraries must be tagged in git, but tag " ++ vsn ++ " was not found."
          )
          [ vcat $ map text $
              [ "These tags make it possible to find this specific version on GitHub."
              , "To tag the most recent commit and push it to GitHub, run this:"
              , ""
              , "    git tag -a " ++ vsn ++ " -m \"release version " ++ vsn ++ "\""
              , "    git push origin " ++ vsn
              ]
          ]

    AlreadyPublished vsn ->
      Message
        ( "Version " ++ Pkg.versionToString vsn ++ " has already been published.\
          \ You cannot publish it again! Run the following command to see what\
          \ the new version should be:"
        )
        [ indent 4 $ text "elm-package bump"
        ]

    Unbumpable vsn versions ->
      let
        list =
          case map Pkg.versionToString versions of
            [v] ->
              " to " ++ v ++ "."

            [v,w] ->
              " to " ++ v ++ " or " ++ w ++ "."

            vsnStrings ->
              " to one of these:  "++ List.intercalate ", " vsnStrings
      in
        Message
          ( "To compute a version bump, I need to start with a version that has\
            \ already been published. Your " ++ Path.description
            ++ " says I should start with version "
            ++ Pkg.versionToString vsn
            ++ ", but I cannot find that version on <http://package.elm-lang.org>."
          )
          [ reflow $
              "Try again after changing the version in " ++ Path.description ++ list
          ]

    InvalidBump statedVersion latestVersion ->
      Message
        ( "Your " ++ Path.description ++ " says the next version should be "
          ++ Pkg.versionToString statedVersion ++ ", but that is not valid\
          \ based on the previously published versions."
        )
        [ reflow $
            "Generally, you want to put the most recently published version ("
            ++ Pkg.versionToString latestVersion ++ " for this package) in your "
            ++ Path.description
            ++ " and run `elm-package bump` to figure out what should come next."
        ]

    BadBump old new magnitude realNew realMagnitude ->
      Message
        ( "Your " ++ Path.description ++ " says the next version should be "
          ++ Pkg.versionToString new ++ ", indicating a " ++ show magnitude
          ++ " change to the public API. This does not match the API diff given by:"
        )
        [ indent 4 $ text $
            "elm-package diff " ++ Pkg.versionToString old

        , reflow $
          "This command says this is a " ++ show realMagnitude
          ++ " change, so the next version should be "
          ++ Pkg.versionToString realNew
          ++ ". Double check everything to make sure you are publishing what you want!"
        , reflow $
            "Also, next time use `elm-package bump` and I'll figure all this out for you!"
        ]


showDependency :: Pkg.Name -> C.Constraint -> String
showDependency name constraint =
    show (Pkg.toString name) ++ ": " ++ show (C.toString constraint)


hintToDoc :: Hint -> Doc
hintToDoc hint =
  case hint of
    EmptyConstraint name constraint ->
      stack
        [ reflow $ "Your " ++ Path.description ++ " has the following dependency:"
        , indent 4 $ text $ showDependency name constraint
        , reflow $
            "But there are no released versions in that range! I recommend\
            \ removing that constraint by hand and adding it back with:"
        , indent 4 $ text $ "elm-package install " ++ Pkg.toString name
        ]

    IncompatibleConstraint name constraint viableVersion ->
      stack
        [ reflow $ "Your " ++ Path.description ++ " has the following dependency:"
        , indent 4 $ text $ showDependency name constraint
        , reflow $
            "But none of the versions in that range work with Elm "
            ++ Pkg.versionToString Compiler.version ++ ". I recommend removing\
            \ that dependency by hand and adding it back with:"
        , indent 4 $
            text ("elm-package install " ++ Pkg.toString name)
            <+> dullyellow (text (Pkg.versionToString viableVersion))
        ]

    IncompatiblePackage name ->
      let
        intro =
          map text $ words $
            "There are no versions of " ++ Pkg.toString name ++ " that work with Elm "
            ++ Pkg.versionToString Compiler.version ++ "."

        outro =
          case name of
            Pkg.Name "evancz" "elm-svg" ->
              instead "elm-lang/svg"

            Pkg.Name "evancz" "elm-html" ->
              instead "elm-lang/html"

            Pkg.Name "evancz" "virtual-dom" ->
              instead "elm-lang/virtual-dom"

            _ ->
              map text (words "Maybe the maintainer has not updated it yet.")
      in
        fillSep $ intro ++ outro


instead :: String -> [Doc]
instead newName =
  map text (words "Remove that constraint and use")
  ++ [ dullyellow (text newName), text "instead!" ]


hintToBullet :: Hint -> Doc
hintToBullet hint =
  dullred (text "-->") <+> align (hintToDoc hint)



-- RENDERERS


toStderr :: Error -> IO ()
toStderr err =
  do  isTerminal <- hIsTerminalDevice stderr
      if isTerminal
        then displayIO stderr (renderPretty 1 80 (toDoc err))
        else hPutStr stderr (toString err)


toString :: Error -> String
toString err =
  displayS (renderPretty 1 80 (plain (toDoc err))) ""


toDoc :: Error -> Doc
toDoc err =
  let
    (Message summary details) =
      toMessage err

    summaryDoc =
      fillSep (errorStart : map text (words summary))
  in
    stack (summaryDoc : details)
    <> hardline
    <> hardline


stack :: [Doc] -> Doc
stack allDocs =
  case allDocs of
    [] ->
      error "Do not use `stack` on empty lists."

    doc : docs ->
      List.foldl' verticalAppend doc docs


verticalAppend :: Doc -> Doc -> Doc
verticalAppend a b =
  a <> hardline <> hardline <> b


errorStart :: Doc
errorStart =
  red (underline (text "Error")) <> text ":"


reflow :: String -> Doc
reflow paragraph =
  fillSep (map text (words paragraph))
