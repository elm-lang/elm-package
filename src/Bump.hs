module Bump where

import Control.Monad.Error (throwError, liftIO)
import qualified Data.List as List

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Diff.Compare as Compare
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as V
import qualified Manager


bump :: Manager.Manager ()
bump =
    do  description <- Desc.read
        let name = Desc.name description
        let statedVersion = Desc.version description

        newDocs <- error "need to generate docs"

        maybeVersions <- Catalog.versions name
        case maybeVersions of
            Nothing -> do
                Cmd.out explanation
                if statedVersion == V.initialVersion
                    then Cmd.out goodMsg
                    else changeVersion badMsg description V.initialVersion

            Just publishedVersions ->
                if statedVersion `elem` publishedVersions
                    then suggestVersion newDocs name statedVersion description
                    else validateVersion newDocs name statedVersion publishedVersions

    where
        explanation =
            unlines
            [ "This package has never been published before. Here's how things work:"
            , ""
            , "  * All packages start with initial version " ++ V.toString V.initialVersion
            , "  * Versions are incremented based on API changes and verified automatically"
            , ""
            ]

        goodMsg =
            "The version number in " ++ Path.description ++ " is correct so you are all set!"

        badMsg =
            unlines
            [ "It looks like the version in " ++ Path.description ++ " has been changed though!"
            , ""
            , "Would you like me to change it back to " ++ V.toString V.initialVersion ++ "? (y/n)"
            ]


changeVersion :: String -> Desc.Description -> V.Version -> Manager.Manager ()
changeVersion explanation description newVersion = 
    do  Cmd.out explanation
        yes <- liftIO Cmd.yesOrNo
        case yes of
            False ->
                Cmd.out "Okay, but it is best to let me change it, so run this command later!"

            True -> do
                liftIO $ Desc.write (description { Desc.version = newVersion })
                Cmd.out "Success!"


suggestVersion :: FilePath -> N.Name -> V.Version -> Desc.Description -> Manager.Manager ()
suggestVersion newDocs name version description =
    do  changes <- Compare.computeChanges newDocs name version
        let newVersion = Compare.bumpBy changes version
        changeVersion (infoMsg changes newVersion) description newVersion

    where
        infoMsg changes newVersion =
            unlines
            [ "Based on your new API, this should be a " ++ show (Compare.packageChangeMagnitude changes) ++ " change."
            , "If you are improving upon " ++ V.toString version ++ ", the new version should be " ++ V.toString newVersion ++ "."
            , ""
            , "Would you like us to make this change to " ++ Path.description ++ " now? (y/n)"
            ]


validateVersion :: FilePath -> N.Name -> V.Version -> [V.Version] -> Manager.Manager ()
validateVersion newDocs name statedVersion publishedVersions =
    case List.find (\(_ ,new, _) -> statedVersion == new) bumps of
        Nothing ->
            throwError invalidBump

        Just (old, new, magnitude) ->
            do  changes <- Compare.computeChanges newDocs name old
                let realNew = Compare.bumpBy changes old
                if new == realNew
                    then Cmd.out (looksGood old new magnitude)
                    else throwError (badBump old new realNew magnitude changes)

    where
        bumps = validBumps publishedVersions

        looksGood old new magnitude =
            "Version number " ++ V.toString new ++ " verified (" ++ show magnitude
            ++ " change, " ++ V.toString old ++ " => " ++ V.toString new ++ ")"

        invalidBump =
            unlines
            [ "Something is off with the version listed in " ++ Path.description ++ "."
            , "The easiest way to bump versions is to let us do it automatically. If you set"
            , "the version number in " ++ Path.description ++ " to the released version"
            , "that you are improving upon, we will compute which version should come next"
            , "when you run:"
            , ""
            , "    elm-package bump"
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
            , "    elm-package bump"
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

