module Bump where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.List as List

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Diff.Compare as Compare
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Package
import qualified Elm.Package.Paths as Path
import qualified Manager


bump :: Manager.Manager ()
bump =
    do  description <- Desc.read Path.description
        let name = Desc.name description
        let statedVersion = Desc.version description

        newDocs <- Docs.generate name

        maybeVersions <- Catalog.versions name
        case maybeVersions of
            Nothing ->
                validateInitialVersion description

            Just publishedVersions ->
                let baseVersions = map (\(old, _, _) -> old) (validBumps publishedVersions) in
                if statedVersion `elem` baseVersions
                    then suggestVersion newDocs name statedVersion description
                    else throwError (unbumpable baseVersions)

        return ()


unbumpable :: [Package.Version] -> String
unbumpable baseVersions =
  let versions = map head (List.group (List.sort baseVersions))
  in
    unlines
    [ "To bump you must start with an already published version number in"
    , Path.description ++ ", giving us a starting point to bump from."
    , ""
    , "The version numbers that can be bumped include the following subset of"
    , "published versions:"
    , "  " ++ List.intercalate ", " (map Package.versionToString versions)
    , ""
    , "Switch back to one of these versions before running 'elm-package bump'"
    , "again."
    ]


data Validity
    = Valid
    | Invalid
    | Changed Package.Version


validateInitialVersion :: Desc.Description -> Manager.Manager Validity
validateInitialVersion description =
    do  Cmd.out explanation
        if Desc.version description == Package.initialVersion
            then Cmd.out goodMsg >> return Valid
            else changeVersion badMsg description Package.initialVersion
    where
        explanation =
            unlines
            [ "This package has never been published before. Here's how things work:"
            , ""
            , "  * Versions all have exactly three parts: MAJOR.MINOR.PATCH"
            , ""
            , "  * All packages start with initial version " ++ Package.versionToString Package.initialVersion
            , ""
            , "  * Versions are incremented based on how the API changes:"
            , ""
            , "        PATCH - the API is the same, no risk of breaking code"
            , "        MINOR - values have been added, existing values are unchanged"
            , "        MAJOR - existing values have been changed or removed"
            , ""
            , "  * I will bump versions for you, automatically enforcing these rules"
            , ""
            ]

        goodMsg =
            "The version number in " ++ Path.description ++ " is correct so you are all set!"

        badMsg =
            concat
            [ "It looks like the version in " ++ Path.description ++ " has been changed though!\n"
            , "Would you like me to change it back to " ++ Package.versionToString Package.initialVersion ++ "? [Y/n] "
            ]


changeVersion :: String -> Desc.Description -> Package.Version -> Manager.Manager Validity
changeVersion explanation description newVersion =
    do  liftIO $ putStr explanation
        yes <- liftIO Cmd.yesOrNo
        case yes of
            False -> do
                Cmd.out "Okay, no changes were made."
                return Invalid

            True -> do
                liftIO $ Desc.write (description { Desc.version = newVersion })
                Cmd.out $ "Version changed to " ++ Package.versionToString newVersion ++ "."
                return (Changed newVersion)


suggestVersion
    :: [Docs.Documentation]
    -> Package.Name
    -> Package.Version
    -> Desc.Description
    -> Manager.Manager Validity
suggestVersion newDocs name version description =
    do  changes <- Compare.computeChanges newDocs name version
        let newVersion = Compare.bumpBy changes version
        changeVersion (infoMsg changes newVersion) description newVersion

    where
        infoMsg changes newVersion =
            let old = Package.versionToString version
                new = Package.versionToString newVersion
                magnitude = show (Compare.packageChangeMagnitude changes)
            in
            concat
            [ "Based on your new API, this should be a ", magnitude, " change (", old, " => ", new, ")\n"
            , "Bail out of this command and run 'elm-package diff' for a full explanation.\n"
            , "\n"
            , "Should I perform the update (", old, " => ", new, ") in ", Path.description, "? [Y/n] "
            ]


validateVersion
    :: [Docs.Documentation]
    -> Package.Name
    -> Package.Version
    -> [Package.Version]
    -> Manager.Manager Validity
validateVersion newDocs name statedVersion publishedVersions =
    case List.find (\(_ ,new, _) -> statedVersion == new) bumps of
        Nothing ->
          let isPublished = statedVersion `elem` publishedVersions
          in
              throwError (if isPublished then alreadyPublished else invalidBump)

        Just (old, new, magnitude) ->
            do  changes <- Compare.computeChanges newDocs name old
                let realNew = Compare.bumpBy changes old
                case new == realNew of
                    False ->
                        throwError (badBump old new realNew magnitude changes)
                    True -> do
                        Cmd.out (looksGood old new magnitude)
                        return Valid

    where
        bumps = validBumps publishedVersions

        looksGood old new magnitude =
            "Version number " ++ Package.versionToString new ++ " verified (" ++ show magnitude
            ++ " change, " ++ Package.versionToString old ++ " => " ++ Package.versionToString new ++ ")"

        alreadyPublished =
            "Version " ++ Package.versionToString statedVersion
            ++ " has already been published, but you are trying to publish\n"
            ++ "it again! Run the following command to see what the new version should be.\n"
            ++ "\n    elm-package bump\n"

        invalidBump =
            unlines
            [ "The version listed in " ++ Path.description ++ " is neither a previously"
            , "published version, nor a valid version bump."
            , ""
            , "Set the version number in " ++ Path.description ++ " to the released version"
            , "that you are improving upon. If you are working on the latest API, that means"
            , "you are modifying version " ++ Package.versionToString (last publishedVersions) ++ "."
            , ""
            , "From there, we can compute which version comes next based on the API changes"
            , "when you run the following command."
            , ""
            , "    elm-package bump"
            ]

        badBump old new realNew magnitude changes =
            unlines
            [ "It looks like you are trying to bump from version " ++ Package.versionToString old ++ " to " ++ Package.versionToString new ++ "."
            , "This implies you are making a " ++ show magnitude ++ " change, but when we compare"
            , "the " ++ Package.versionToString old ++ " API to the API you have now it seems that it should"
            , "really be a " ++ show (Compare.packageChangeMagnitude changes) ++ " change (" ++ Package.versionToString realNew ++ ")."
            , ""
            , "Run the following command to see the API diff we are working from:"
            , ""
            , "    elm-package diff " ++ Package.versionToString old
            , ""
            , "The easiest way to bump versions is to let us do it automatically. If you set"
            , "the version number in " ++ Path.description ++ " to the released version"
            , "that you are improving upon, we will compute which version should come next"
            , "when you run:"
            , ""
            , "    elm-package bump"
            ]


-- VALID BUMPS

validBumps :: [Package.Version] -> [(Package.Version, Package.Version, Compare.Magnitude)]
validBumps publishedVersions =
    [ (majorPoint, Package.bumpMajor majorPoint, Compare.MAJOR) ]
    ++ map (\v -> (v, Package.bumpMinor v, Compare.MINOR)) minorPoints
    ++ map (\v -> (v, Package.bumpPatch v, Compare.PATCH)) patchPoints
  where
    patchPoints = Package.filterLatest Package.majorAndMinor publishedVersions
    minorPoints = Package.filterLatest Package._major publishedVersions
    majorPoint = head publishedVersions

