module Diff where

import Control.Monad.Error.Class (throwError)

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Diff.Compare as Compare
import qualified Diff.Display as Display
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as V
import qualified Manager


data Range
    = LatestVsActual
    | Since V.Version
    | Between N.Name V.Version V.Version


diff :: Range -> Manager.Manager ()
diff range =
    case range of
        LatestVsActual ->
            do  desc <- Desc.read Path.description
                let name = Desc.name desc
                newDocs <- Docs.generate desc

                maybeVersions <- Catalog.versions name
                latestVersion <-
                    maybe (throwError noVersions) (return . maximum) maybeVersions

                computeDiff name latestVersion newDocs Nothing

        Since version ->
            do  desc <- Desc.read Path.description
                newDocs <- Docs.generate desc
                computeDiff (Desc.name desc) version newDocs Nothing

        Between name old new ->
            do  newDocs <- Catalog.documentation name new
                computeDiff name old newDocs (Just new)


noVersions :: String
noVersions =
    "This package has not been published, there is nothing to diff against!"


computeDiff
    :: N.Name
    -> V.Version
    -> [Docs.Documentation]
    -> Maybe V.Version
    -> Manager.Manager ()
computeDiff name oldVersion newDocs maybeNewVersion =
    do  Cmd.out msg
        changes <- Compare.computeChanges newDocs name oldVersion
        Cmd.out (Display.packageChanges changes)
    where
        msg =
            "Comparing " ++ N.toString name ++ " " ++ V.toString oldVersion ++ " to " ++ newStuff ++ "..."

        newStuff =
            case maybeNewVersion of
                Nothing -> "local changes"
                Just version -> V.toString version
