module Bump where

import qualified Data.List as List

import qualified Catalog
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as P
import qualified Elm.Package.Version as V
import qualified Manager


whatIsNext :: N.Name -> Manager.Manager V.Version
whatIsNext name =
    do  description <- Desc.read
        let statedVersion = Desc.version description

        newDocs <- error "need to generate docs"

        publishedVersions <- Catalog.versions name
        if statedVersion `elem` publishedVersions
            then suggestVersion newDocs name statedVersion description
            else validateVersion newDocs name statedVersion publishedVersions


suggestVersion :: FilePath -> N.Name -> V.Version -> Desc.Description -> Manager.Manager ()
suggestVersion newDocs name version description =
    do  changes <- computeChanges newDocs name version
        let newVersion = Compare.bumpBy changes version
        Cmd.out (infoMsg changes)
        yes <- Cmd.yesOrNo
        case yes of
            False ->
                Cmd.out "Okay, but it is best to let me change it, so run this command later!"

            True ->
                do  Desc.write (description { version = newVersion })
                    Cmd.out "Success!"

    where
        infoMsg changes newVersion =
            unlines
            [ "Based on your new API, this should be a " ++ show (Compare.packageChangeMagnitude changes) ++ " change."
            , "That means you need to upgrade from version " ++ V.toString version ++ " to " ++ V.toString newVersion ++ "."
            , ""
            , "Would you like us to make this change to " ++ Path.description ++ " now? (y/n)"
            ]


validateVersion :: FilePath -> N.Name -> V.Version -> [V.Version] -> Manager.Manager ()
validateVersion newDocs name statedVersion publishedVersions =
    do  bumps <- validBumps publishedVersions
        case List.find (\(_ ,new, _) -> statedVersion == new) bumps of
            Nothing ->
                throwError invalidBump

            Just (old, new, magnitude) ->
                do  changes <- computeChanges newDocs name old
                    let realNew = Compare.bumpBy changes old
                    if new == realNew
                        then return new
                        else throwError (badBump old new realNew magnitude changes)

    where
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


computeChanges :: FilePath -> N.Name -> V.Version -> Manager.Manager Compare.PackageChanges
computeChanges newDocs name version =
    do  oldDocs <- Catalog.docs name version
        old <- BS.readFile oldDocs
        new <- BS.readFile newDocs
        case (,) <$> Json.eitherDecode old <*> Json.eitherDecode new of
            Left msg ->
                throwError msg

            Right (oldPackage, newPackage) ->
                return (Compare.diffPackage oldPackage newPackage)


-- VALID BUMPS

validBumps :: [V.Version] -> [(V.Version, V.Version, Compare.Magnitude)]
validBumps publishedVersions =
    [ (majorPoint, V.bumpMajor majorPoint, Compare.MAJOR) ]
    ++ map (\v -> (v, V.bumpMinor v, Compare.MINOR) minorPoints
    ++ map (\v -> (v, V.bumpPatch v, Compare.PATCH) patchPoints
  where
    patchPoints = V.filterLatest V.majorAndMinor publishedVersions
    minorPoints = V.filterLatest V.major publishedVersions
    majorPoint = last publishedVersions

