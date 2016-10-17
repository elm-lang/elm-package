module Install where

import Control.Monad.Except (liftIO, throwError)
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Package as Package
import qualified Elm.Package.Constraint as Constraint
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Install.Fetch as Fetch
import qualified Install.Plan as Plan
import qualified Install.Solver as Solver
import qualified Manager
import qualified Reporting.Error as Error
import qualified Store



data Args
    = Everything
    | Latest Package.Name
    | Exactly Package.Name Package.Version


install :: Bool -> Args -> Manager.Manager ()
install autoYes args =
  do  exists <- liftIO (doesFileExist Path.description)

      description <-
        if exists then
          Desc.read Error.CorruptDescription Path.description
        else
          initialDescription

      case args of
        Everything ->
            upgrade autoYes description

        Latest name ->
            do  version <- latestVersion name
                newDescription <- addConstraint autoYes name version description
                upgrade autoYes newDescription

        Exactly name version ->
            do  newDescription <- addConstraint autoYes name version description
                upgrade autoYes newDescription



-- INSTALL EVERYTHING


upgrade :: Bool -> Desc.Description -> Manager.Manager ()
upgrade autoYes desc =
  do  newSolution <- Solver.solve (Desc.elmVersion desc) (Desc.dependencies desc)

      exists <- liftIO (doesFileExist Path.solvedDependencies)

      oldSolution <-
        if exists then
          Solution.read Error.CorruptSolution Path.solvedDependencies
        else
          return Map.empty

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
      do  putStrLn "Some new packages are needed. Here is the upgrade plan."
          putStrLn (Plan.display plan)
          putStr "Do you approve of this plan? [Y/n] "
          Cmd.yesOrNo


runPlan :: Solution.Solution -> Plan.Plan -> Manager.Manager ()
runPlan solution plan =
  do  let installs =
            Map.toList (Plan.installs plan)
            ++ Map.toList (Map.map snd (Plan.upgrades plan))

      let removals =
            Map.toList (Plan.removals plan)
            ++ Map.toList (Map.map fst (Plan.upgrades plan))

      -- ensure we have the stuff directory before doing anything else
      liftIO (createDirectoryIfMissing True Path.stuffDirectory)

      -- fetch new dependencies
      Fetch.everything installs

      -- try to build new dependencies
      liftIO (Solution.write Path.solvedDependencies solution)

      -- remove dependencies that are not needed
      Cmd.inDir Path.packagesDirectory $
          forM_ removals $ \(name, version) ->
              liftIO $
                removeDirectoryRecursive (Package.toFilePath name </> Package.versionToString version)

      liftIO $ putStrLn "Packages configured successfully!"



-- MODIFY DESCRIPTION


latestVersion :: Package.Name -> Manager.Manager Package.Version
latestVersion name =
  do  versionCache <- Store.readVersionCache
      case Map.lookup name versionCache of
        Just versions ->
          return $ maximum versions

        Nothing ->
          throwError $ Error.PackageNotFound name $
            Error.nearbyNames name (Map.keys versionCache)


addConstraint :: Bool -> Package.Name -> Package.Version -> Desc.Description -> Manager.Manager Desc.Description
addConstraint autoYes name version description =
  case List.lookup name (Desc.dependencies description) of
    Nothing ->
      addNewDependency autoYes name version description

    Just constraint ->
      if Constraint.isSatisfied constraint version then
          return description

      else
        throwError $ Error.AddTrickyConstraint name version constraint


addNewDependency :: Bool -> Package.Name -> Package.Version -> Desc.Description -> Manager.Manager Desc.Description
addNewDependency autoYes name version description =
  do  confirm <-
          case autoYes of
            True -> return True
            False ->
              do  answer <- liftIO confirmNewAddition
                  liftIO (putStrLn "")
                  return answer

      case confirm of
        False ->
          do  liftIO $ putStrLn noConfirmation
              return description
        True ->
          do  let newDescription = description { Desc.dependencies = newConstraints }
              liftIO $ Desc.write newDescription
              return newDescription
  where
    newConstraint =
        Constraint.untilNextMajor version

    newConstraints =
        (name, newConstraint) : Desc.dependencies description

    noConfirmation =
        "Cannot install the new package unless it appears in " ++ Path.description ++ ".\n" ++
        "If you do not like the constraint I suggested, change it manually and then run:\n" ++
        "\n    elm-package install\n\n" ++
        "This will install everything listed in " ++ Path.description ++ "."

    confirmNewAddition =
      do  putStrLn $
            "To install " ++ Package.toString name ++ " I would like to add the following\n"
            ++ "dependency to " ++ Path.description ++ ":\n\n    "
            ++ showDependency name newConstraint
            ++ "\n"

          putStr $ "May I add that to " ++ Path.description ++ " for you? [Y/n] "
          Cmd.yesOrNo


showDependency :: Package.Name -> Constraint.Constraint -> String
showDependency name constraint =
    show (Package.toString name) ++ ": " ++ show (Constraint.toString constraint)


initialDescription :: Manager.Manager Desc.Description
initialDescription =
  do  let core = Package.Name "elm-lang" "core"
      let html = Package.Name "elm-lang" "html"
      coreVersion <- latestVersion core
      htmlVersion <- latestVersion html
      let desc = Desc.defaultDescription {
          Desc.dependencies =
            [ (core, Constraint.untilNextMajor coreVersion)
            , (html, Constraint.untilNextMajor htmlVersion)
            ]
      }
      liftIO (Desc.write desc)
      return desc
