module Diff where

import qualified Data.List as List

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Diff.Compare as Compare
import qualified Diff.Display as Display
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V
import qualified Manager


data Range
    = StatedVsActual
    | Since V.Version
    | Between N.Name V.Version V.Version


diff :: Range -> Manager.Manager ()
diff range =
    case range of
        StatedVsActual ->
            do  desc <- Desc.read
                computeDiff (Desc.name desc) (Desc.version desc) Nothing

        Since version ->
            do  desc <- Desc.read
                computeDiff (Desc.name desc) version Nothing

        Between name old new ->
            computeDiff name old (Just new)


computeDiff :: N.Name -> V.Version -> Maybe V.Version -> Manager.Manager ()
computeDiff name oldVersion maybeNewVersion =
    do  Cmd.out msg
        newDocs <- error "need to generate docs"
        changes <- Compare.computeChanges newDocs name oldVersion
        Cmd.out (Display.packageChanges changes)
    where
        msg =
            "Comparing " ++ N.toString name ++ " " ++ V.toString oldVersion ++ " to " ++ newStuff ++ "..."

        newStuff =
            case maybeNewVersion of
                Nothing -> "local changes"
                Just version -> V.toString version