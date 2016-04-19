module Store (Store, getConstraints, getVersions, initialStore, readVersionCache) where

import Control.Monad.Except (throwError)
import Control.Monad.State (StateT)
import Control.Monad.RWS (lift, liftIO, asks, gets, modify)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Catalog
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Package
import qualified Manager



-- STORE


data Store = Store
    { constraintCache :: ConstraintCache
    , versionCache :: VersionCache
    }


type ConstraintCache =
    Map.Map (Package.Name, Package.Version) (C.Constraint, [(Package.Name, C.Constraint)])


type VersionCache =
    Map.Map Package.Name [Package.Version]


initialStore :: Manager.Manager Store
initialStore =
  do  versionCache <- readVersionCache
      return (Store Map.empty versionCache)


readVersionCache :: Manager.Manager VersionCache
readVersionCache =
  do  cacheDirectory <- asks Manager.cacheDirectory
      let versionsFile = cacheDirectory </> "versions.dat"
      let lastUpdatedPath = cacheDirectory </> "last-updated"

      now <- liftIO Time.getCurrentTime

      exists <- liftIO (Dir.doesFileExist lastUpdatedPath)
      maybeTime <-
          case exists of
            False -> return Nothing
            True ->
              do  rawTime <- liftIO (readFile lastUpdatedPath)
                  return $ Just (read rawTime)

      maybePackages <- Catalog.allPackages maybeTime

      case maybePackages of
        Nothing ->
          do  exists <- liftIO (Dir.doesFileExist versionsFile)
              case exists of
                False -> return Map.empty
                True ->
                  do  binary <- liftIO (BS.readFile versionsFile)
                      return (Binary.decode binary)

        Just packages ->
          let cache :: VersionCache
              cache = Map.fromList packages
          in
              do  liftIO $ BS.writeFile versionsFile (Binary.encode cache)
                  liftIO $ writeFile lastUpdatedPath (show now)
                  return cache



-- CONSTRAINTS


type Explorer =
  StateT Store.Store Manager.Manager


getConstraints :: Package.Name -> Package.Version -> Explorer (C.Constraint, [(Package.Name, C.Constraint)])
getConstraints name version =
  do  cache <- gets constraintCache
      case Map.lookup (name, version) cache of
        Just constraints ->
          return constraints

        Nothing ->
          do  desc <- lift $ Catalog.description name version
              let constraints = (Desc.elmVersion desc, Desc.dependencies desc)
              modify $ \store ->
                  store {
                      constraintCache =
                          Map.insert (name, version) constraints (constraintCache store)
                  }
              return constraints



-- VERSIONS


getVersions :: Package.Name -> Explorer [Package.Version]
getVersions name =
  do  cache <- gets versionCache
      case Map.lookup name cache of
        Just versions ->
          return versions

        Nothing ->
          throwError noLocalVersions
  where
    noLocalVersions =
        "There are no versions of package '" ++ Package.toString name ++ "' on your computer."
