module Diff where

import Control.Monad.Error.Class (throwError)

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Diff.Compare as Compare
import qualified Diff.Display as Display
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Elm.Package as Package
import qualified Manager


data Range
    = LatestVsActual
    | Since Package.Version
    | Between Package.Name Package.Version Package.Version


diff :: Range -> Manager.Manager ()
diff range =
    case range of
        LatestVsActual ->
            do  name <- Desc.name `fmap` Desc.read Path.description
                newDocs <- Docs.generate

                maybeVersions <- Catalog.versions name
                latestVersion <-
                    maybe (throwError noVersions) (return . maximum) maybeVersions

                computeDiff name latestVersion newDocs Nothing

        Since version ->
            do  name <- Desc.name `fmap` Desc.read Path.description
                newDocs <- Docs.generate
                computeDiff name version newDocs Nothing

        Between name old new ->
            do  newDocs <- Catalog.documentation name new
                computeDiff name old newDocs (Just new)


noVersions :: String
noVersions =
    "This package has not been published, there is nothing to diff against!"


computeDiff
    :: Package.Name
    -> Package.Version
    -> [Docs.Documentation]
    -> Maybe Package.Version
    -> Manager.Manager ()
computeDiff name oldVersion newDocs maybeNewVersion =
    do  Cmd.out msg
        changes <- Compare.computeChanges newDocs name oldVersion
        Cmd.out (Display.packageChanges changes)
    where
        msg =
            "Comparing " ++ Package.toString name ++ " " ++ Package.versionToString oldVersion ++ " to " ++ newStuff ++ "..."

        newStuff =
            case maybeNewVersion of
                Nothing -> "local changes"
                Just version -> Package.versionToString version
