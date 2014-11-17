module Bump where

import Control.Monad.Error (throwError, liftIO)
import qualified Data.List as List

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Diff.Compare as Compare
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as V
import qualified Manager


bump :: Manager.Manager ()
bump =
    do  description <- Desc.read Path.description
        let name = Desc.name description
        let statedVersion = Desc.version description

        newDocs <- Docs.generate description

        maybeVersions <- Catalog.versions name
        case maybeVersions of
            Nothing -> 
                validateInitialVersion description

            Just publishedVersions ->
                let bumps = map (\(old, _, _) -> old) (validBumps publishedVersions) in
                if statedVersion `elem` bumps
                    then suggestVersion newDocs name statedVersion description
                    else throwError (unbumpable bumps)

        return ()


unbumpable :: [V.Version] -> String
unbumpable validBumps =
    unlines
    [ "To use bump-version you must start with an already published version number"
    , "in " ++ Path.description ++ ", giving us a starting point to bump from."
    , ""
    , "The version numbers that can be bumped include the following subset of"
    , "published versions:"
    , "  " ++ List.intercalate ", " (map V.toString validBumps)
    , ""
    , "Switch back to one of these versions before running 'elm-package bump-version'"
    , "again."
    ]


data Validity
    = Valid
    | Invalid
    | Changed V.Version


validateInitialVersion :: Desc.Description -> Manager.Manager Validity
validateInitialVersion description =
    do  Cmd.out explanation
        if Desc.version description == V.initialVersion
            then Cmd.out goodMsg >> return Valid
            else changeVersion badMsg description V.initialVersion
    where
        explanation =
            unlines
            [ "This package has never been published before. Here's how things work:"
            , ""
            , "  * Versions all have exactly three parts: MAJOR.MINOR.PATCH"
            , ""
            , "  * Versions are incremented based on how the API changes:"
            , "        PATCH - the API is the same, no risk of breaking code"
            , "        MINOR - values have been added, existing values are unchanged"
            , "        MAJOR - existing values have been changed or removed"
            , ""
            , "  * All packages start with initial version " ++ V.toString V.initialVersion
            , ""
            , "  * I will bump versions for you, automatically enforcing these rules"
            , ""
            ]

        goodMsg =
            "The version number in " ++ Path.description ++ " is correct so you are all set!"

        badMsg =
            concat
            [ "It looks like the version in " ++ Path.description ++ " has been changed though!\n"
            , "Would you like me to change it back to " ++ V.toString V.initialVersion ++ "? (y/n) "
            ]


changeVersion :: String -> Desc.Description -> V.Version -> Manager.Manager Validity
changeVersion explanation description newVersion = 
    do  liftIO $ putStr explanation
        yes <- liftIO Cmd.yesOrNo
        case yes of
            False -> do
                Cmd.out "Okay, no changes were made."
                return Invalid

            True -> do
                liftIO $ Desc.write (description { Desc.version = newVersion })
                Cmd.out $ "Version changed to " ++ V.toString newVersion ++ "."
                return (Changed newVersion)


suggestVersion
    :: [Docs.Documentation]
    -> N.Name
    -> V.Version
    -> Desc.Description
    -> Manager.Manager Validity
suggestVersion newDocs name version description =
    do  changes <- Compare.computeChanges newDocs name version
        let newVersion = Compare.bumpBy changes version
        changeVersion (infoMsg changes newVersion) description newVersion

    where
        infoMsg changes newVersion =
            let old = V.toString version
                new = V.toString newVersion
                magnitude = show (Compare.packageChangeMagnitude changes)
            in
            concat
            [ "Based on your new API, this should be a ", magnitude, " change (", old, " => ", new, ")\n"
            , "Bail out of this command and run 'elm-package diff' for a full explanation.\n"
            , "\n"
            , "Should I perform the update (", old, " => ", new, ") in ", Path.description, "? (y/n) "
            ]


validateVersion
    :: [Docs.Documentation]
    -> N.Name
    -> V.Version
    -> [V.Version]
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
            "Version number " ++ V.toString new ++ " verified (" ++ show magnitude
            ++ " change, " ++ V.toString old ++ " => " ++ V.toString new ++ ")"

        alreadyPublished =
            "Version " ++ V.toString statedVersion
            ++ " has already been published, but you are trying to publish\n"
            ++ "it again! Run the following command to see what the new version should be.\n"
            ++ "\n    elm-package bump-version\n"

        invalidBump =
            unlines
            [ "The version listed in " ++ Path.description ++ " is neither a previously"
            , "published version, nor a valid version bump."
            , ""
            , "Set the version number in " ++ Path.description ++ " to the released version"
            , "that you are improving upon. If you are working on the latest API, that means"
            , "you are modifying version " ++ V.toString (last publishedVersions) ++ "."
            , ""
            , "From there, we can compute which version comes next based on the API changes"
            , "when you run the following command."
            , ""
            , "    elm-package bump-version"
            ]

        badBump old new realNew magnitude changes =
            unlines
            [ "It looks like you are trying to bump from version " ++ V.toString old ++ " to " ++ V.toString new ++ "."
            , "This implies you are making a " ++ show magnitude ++ " change, but when we compare"
            , "the " ++ V.toString old ++ " API to the API you have now it seems that it should"
            , "really be a " ++ show (Compare.packageChangeMagnitude changes) ++ " change (" ++ V.toString realNew ++ ")."
            , ""
            , "Run the following command to see the API diff we are working from:"
            , ""
            , "    elm-package diff " ++ V.toString old
            , ""
            , "The easiest way to bump versions is to let us do it automatically. If you set"
            , "the version number in " ++ Path.description ++ " to the released version"
            , "that you are improving upon, we will compute which version should come next"
            , "when you run:"
            , ""
            , "    elm-package bump-version"
            ]


-- VALID BUMPS

validBumps :: [V.Version] -> [(V.Version, V.Version, Compare.Magnitude)]
validBumps publishedVersions =
    [ (majorPoint, V.bumpMajor majorPoint, Compare.MAJOR) ]
    ++ map (\v -> (v, V.bumpMinor v, Compare.MINOR)) minorPoints
    ++ map (\v -> (v, V.bumpPatch v, Compare.PATCH)) patchPoints
  where
    patchPoints = V.filterLatest V.majorAndMinor publishedVersions
    minorPoints = V.filterLatest V.major publishedVersions
    majorPoint = last publishedVersions

