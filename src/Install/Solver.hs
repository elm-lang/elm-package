module Install.Solver where

import Control.Monad.Error (throwError)
import Control.Monad.State (StateT, evalStateT)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Name as N
import qualified Elm.Package.Solution as S
import qualified Elm.Package.Version as V
import qualified Manager
import qualified Store


solve :: [(N.Name, C.Constraint)] -> Manager.Manager S.Solution
solve constraints =
    do  store <- Store.initialStore constraints

        let explorer =
                exploreVersion N.dummyName V.dummyVersion Map.empty Map.empty

        maybeSolution <- evalStateT explorer store
        case maybeSolution of
          Just solution -> return solution
          Nothing ->
              throwError $
              "Unable to find a set of packages that will work with your constraints.\n\
              \Try running the following command to get the latest list of packages:\n\n\
              \    elm-package update\n\n\
              \This may pull in additional versions that will make it possible to install."


-- EXPLORE CONSTRAINTS

type Explorer a =
    StateT Store.Store Manager.Manager a


type Packages =
    Map.Map N.Name [V.Version]


explorePackages :: S.Solution -> Packages -> Explorer (Maybe S.Solution)
explorePackages solution availablePackages =
    case Map.minViewWithKey availablePackages of
      Nothing ->
          return (Just solution)

      Just ((name, versions), remainingPackages) ->
          exploreVersionList name versions solution remainingPackages


exploreVersionList :: N.Name -> [V.Version] -> S.Solution -> Packages -> Explorer (Maybe S.Solution)
exploreVersionList name versions solution remainingPackages =
    go (reverse (V.filterLatest V.majorAndMinor versions))
  where
    go versions =
        case versions of
          [] -> return Nothing
          version : rest ->
              do  maybeSolution <- exploreVersion name version solution remainingPackages
                  case maybeSolution of
                    Nothing -> go rest
                    answer -> return answer


exploreVersion :: N.Name -> V.Version -> S.Solution -> Packages -> Explorer (Maybe S.Solution)
exploreVersion name version solution remainingPackages =
  do  constraints <- Store.getConstraints name version

      let (overlappingConstraints, newConstraints) =
              List.partition (\(name, _) -> Map.member name solution) constraints

      case all (satisfiedBy solution) overlappingConstraints of
        False -> return Nothing
        True ->
          do  maybePackages <- addConstraints remainingPackages newConstraints
              case maybePackages of
                Nothing -> return Nothing
                Just extendedPackages ->
                    explorePackages (Map.insert name version solution) extendedPackages


satisfiedBy :: S.Solution -> (N.Name, C.Constraint) -> Bool
satisfiedBy solution (name, constraint) =
    case Map.lookup name solution of
      Nothing -> False
      Just version ->
          C.isSatisfied constraint version


addConstraints :: Packages -> [(N.Name, C.Constraint)] -> Explorer (Maybe Packages)
addConstraints packages constraints =
    case constraints of
      [] -> return (Just packages)
      (name, constraint) : rest ->
          do  versions <- Store.getVersions name
              case filter (C.isSatisfied constraint) versions of
                [] -> return Nothing
                vs -> addConstraints (Map.insert name vs packages) rest
