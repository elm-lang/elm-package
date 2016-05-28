module Bump where

import Control.Monad.Except (liftIO, throwError)
import qualified Data.List as List

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Diff.Compare as Diff
import qualified Diff.Magnitude as Diff
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Package
import qualified Elm.Package.Paths as Path
import qualified Manager
import qualified Reporting.Error as Error


bump :: Manager.Manager ()
bump =
    do  description <- Desc.read Error.CorruptDescription Path.description
        let name = Desc.name description
        let statedVersion = Desc.version description

        newDocs <- Docs.generate name

        maybeVersions <- Catalog.versions name
        case maybeVersions of
            Nothing ->
                validateInitialVersion description

            Just publishedVersions ->
              let
                baseVersions =
                  map (\(old, _, _) -> old) (validBumps publishedVersions)
              in
                if statedVersion `elem` baseVersions then
                  suggestVersion newDocs name statedVersion description
                else
                  throwError $ Error.Unbumpable statedVersion $
                    map head (List.group (List.sort baseVersions))

        return ()


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
    do  changes <- Diff.computeChanges newDocs name version
        let newVersion = Diff.bumpBy changes version
        changeVersion (infoMsg changes newVersion) description newVersion

    where
        infoMsg changes newVersion =
            let old = Package.versionToString version
                new = Package.versionToString newVersion
                magnitude = show (Diff.packageChangeMagnitude changes)
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
          if elem statedVersion publishedVersions then
            throwError $ Error.AlreadyPublished statedVersion

          else
            throwError $ Error.InvalidBump statedVersion (last publishedVersions)

        Just (old, new, magnitude) ->
            do  changes <- Diff.computeChanges newDocs name old
                let realNew = Diff.bumpBy changes old
                case new == realNew of
                    False ->
                      throwError $ Error.BadBump old new magnitude realNew $
                        Diff.packageChangeMagnitude changes

                    True ->
                      do  Cmd.out (looksGood old new magnitude)
                          return Valid

    where
        bumps =
            validBumps publishedVersions

        looksGood old new magnitude =
            "Version number " ++ Package.versionToString new ++ " verified (" ++ show magnitude
            ++ " change, " ++ Package.versionToString old ++ " => " ++ Package.versionToString new ++ ")"



-- VALID BUMPS


validBumps :: [Package.Version] -> [(Package.Version, Package.Version, Diff.Magnitude)]
validBumps publishedVersions =
    [ (majorPoint, Package.bumpMajor majorPoint, Diff.MAJOR) ]
    ++ map (\v -> (v, Package.bumpMinor v, Diff.MINOR)) minorPoints
    ++ map (\v -> (v, Package.bumpPatch v, Diff.PATCH)) patchPoints
  where
    patchPoints = Package.filterLatest Package.majorAndMinor publishedVersions
    minorPoints = Package.filterLatest Package._major publishedVersions
    majorPoint = head publishedVersions

