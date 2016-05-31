{-# OPTIONS_GHC -Wall #-}
module Reporting.Error
  ( Error(..)
  , toString
  , toStderr
  , nearbyNames
  )
  where

import Data.Function (on)
import qualified Data.List as List
import qualified Elm.Package as Pkg
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Paths as Path
import GHC.IO.Handle (hIsTerminalDevice)
import System.IO (hPutStr, stderr)
import qualified Text.EditDistance as Dist
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), displayS, displayIO, fillSep, hardline, indent, plain
  , red, renderPretty, text, underline, vcat
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
  | ConstraintsHaveNoSolution
  | PackageNotFound Pkg.Name [Pkg.Name]
  | AddTrickyConstraint Pkg.Name Pkg.Version C.Constraint

  | BadInstall Pkg.Version

  | Undiffable
  | InvalidVersion
  | BadMetadata [String]
  | MissingTag Pkg.Version

  | AlreadyPublished Pkg.Version
  | Unbumpable Pkg.Version [Pkg.Version]
  | InvalidBump Pkg.Version Pkg.Version
  | BadBump Pkg.Version Pkg.Version Diff.Magnitude Pkg.Version Diff.Magnitude



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

    CorruptJson _ _ _ ->
      error "TODO - CorruptJson"

    CorruptDescription _ ->
      error "TODO - CorruptDescription"

    CorruptDocumentation _ ->
      error "TODO - CorruptDocumentation"

    CorruptSolution _ ->
      error "TODO - CorruptSolution"

    CorruptVersionCache _ ->
      error "TODO - CorruptVersionCache"

    ConstraintsHaveNoSolution ->
      error "TODO - ConstraintsHaveNoSolution"

    PackageNotFound package suggestions ->
      Message
        ( "Could not find any packages named " ++ Pkg.toString package ++ "."
        )
        [ text $ "Here are some packages that have similar names:"
        , indent 4 $ vcat $ map (text . Pkg.toString) suggestions
        , text $ "Maybe you want one of those?"
        ]

    AddTrickyConstraint name version constraint ->
      error "TODO" $
      "This is a tricky update, you should modify " ++ Path.description ++ " yourself.\n"
      ++ "Package " ++ Pkg.toString name ++ " is already listed as a dependency:\n\n    "
      ++ showDependency name constraint ++ "\n\n"
      ++ "You probably want one of the following constraints instead:\n\n    "
      ++ C.toString (C.expand constraint version) ++ "\n    "
      ++ C.toString (C.untilNextMajor version) ++ "\n"

    BadInstall version ->
      Message
        ( "You specified a version number, but not a package! Version "
          ++ Pkg.versionToString version ++ " of what?"
        )
        []

    Undiffable ->
      error "TODO" "This package has not been published, there is nothing to diff against!"

    InvalidVersion ->
      error "TODO" "Cannot publish with an invalid version!"

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

    verticalAppend a b =
      a <> hardline <> hardline <> b
  in
    List.foldl' verticalAppend summaryDoc details
    <> hardline
    <> hardline


errorStart :: Doc
errorStart =
  red (underline (text "Error")) <> text ":"


reflow :: String -> Doc
reflow paragraph =
  fillSep (map text (words paragraph))
