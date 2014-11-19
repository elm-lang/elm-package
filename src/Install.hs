module Install where

import Control.Monad.Error
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Package.Constraint as Constraint
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Version as V
import qualified Install.Fetch as Fetch
import qualified Install.Plan as Plan
import qualified Install.Solver as Solver
import qualified Manager
import qualified Store


data Args
    = Everything
    | Latest N.Name
    | Exactly N.Name V.Version


install :: Bool -> Args -> Manager.Manager ()
install autoYes args =
    case args of
      Everything ->
          upgrade autoYes

      Latest name ->
          do  version <- latestVersion name
              updateDescription autoYes name version
              upgrade autoYes

      Exactly name version ->
          do  updateDescription autoYes name version
              upgrade autoYes


-- INSTALL EVERYTHING

upgrade :: Bool -> Manager.Manager ()
upgrade autoYes =
  do  description <- Desc.read Path.description

      newSolution <- Solver.solve (Desc.dependencies description)

      exists <- liftIO (doesFileExist Path.solvedDependencies)
      oldSolution <-
          if exists
              then Solution.read Path.solvedDependencies
              else return Map.empty

      let plan = Plan.create oldSolution newSolution

      approve <- liftIO (getApproval autoYes plan)

      if approve
          then runPlan newSolution plan
          else liftIO $ putStrLn "Okay, I did not change anything!"            


getApproval :: Bool -> Plan.Plan -> IO Bool
getApproval autoYes plan =
  case autoYes || Plan.isEmpty plan of
    True ->
      return True

    False ->
      do  putStrLn "To install we must make the following changes:"
          putStrLn (Plan.display plan)
          putStr "Do you approve of this plan? (y/n) "
          Cmd.yesOrNo


runPlan :: Solution.Solution -> Plan.Plan -> Manager.Manager ()
runPlan solution plan =
  do  -- fetch new dependencies
      Cmd.inDir Path.packagesDirectory $
          mapM_ (uncurry Fetch.package) installs

      -- try to build new dependencies
      liftIO (Solution.write Path.solvedDependencies solution)

      -- remove dependencies that are not needed
      Cmd.inDir Path.packagesDirectory $
          liftIO $ mapM_ remove removals

      liftIO $ putStrLn "Packages configured successfully!"
  where
    installs =
        Map.toList (Plan.installs plan)
        ++ Map.toList (Map.map snd (Plan.upgrades plan))

    removals =
        Map.toList (Plan.removals plan)
        ++ Map.toList (Map.map fst (Plan.upgrades plan))

    remove (name, version) =
        removeDirectoryRecursive (N.toFilePath name </> V.toString version)


-- MODIFY DESCRIPTION

updateDescription :: Bool -> N.Name -> V.Version -> Manager.Manager ()
updateDescription autoYes name version =
  do  exists <- liftIO (doesFileExist Path.description)

      desc <-
          case exists of
            True -> Desc.read Path.description
            False -> return Desc.defaultDescription

      addConstraint autoYes desc name version


latestVersion :: N.Name -> Manager.Manager V.Version
latestVersion name =
  do  versionCache <- Store.readVersionCache
      case Map.lookup name versionCache of
        Just versions ->
            return $ maximum versions

        Nothing ->
            throwError $
            unlines
            [ "No versions of package '" ++ N.toString name ++ "' were found!"
            , "Is it spelled correctly? If so, try running the following command to download"
            , "the latest package listing to your computer:"
            , ""
            , "    elm-package update"
            , ""
            , "After that, try installing again."
            ]


addConstraint :: Bool -> Desc.Description -> N.Name -> V.Version -> Manager.Manager ()
addConstraint autoYes description name version =
  do  confirm <-
          case autoYes of
            True -> return True
            False -> liftIO confirmChange

      case confirm of
        False -> throwError noConfirmation
        True ->
            liftIO $ Desc.write $ description { Desc.dependencies = newConstraints }

  where
    newConstraints =
        List.insertBy
            (compare `on` fst)
            (name, Constraint.exactly version)
            (Desc.dependencies description)

    noConfirmation =
        "Cannot install the new package without changing " ++ Path.description ++ ".\n" ++
        "It may be easiest to modify it manually and then run 'elm-package install'."

    confirmChange =
        do  putStrLn $ "I am about to add " ++ N.toString name ++ " " ++ V.toString version ++ " to " ++ Path.description
            case List.lookup name (Desc.dependencies description) of
              Nothing -> return ()
              Just constraint ->
                  putStrLn $ "This will replace the existing constraint \"" ++ Constraint.toString constraint ++ "\""

            putStr "Would you like to proceed? (y/n) "
            Cmd.yesOrNo
