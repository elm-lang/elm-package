{-# LANGUAGE FlexibleContexts #-}
module Store (Store, getConstraints, getVersions, initialStore, readVersionCache) where

import Control.Monad.Error (MonadError, throwError)
import Control.Monad.RWS (MonadIO, liftIO, MonadReader, ask, asks, MonadState, gets, modify)
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir
import System.FilePath ((</>), dropFileName)

import qualified Catalog
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as P
import qualified Elm.Package.Version as V
import qualified Manager
import qualified Utils.Http as Http


-- STORE

data Store = Store
    { constraintCache :: ConstraintCache
    , versionCache :: VersionCache
    }

type ConstraintCache =
    Map.Map (N.Name, V.Version) [(N.Name, C.Constraint)]

type VersionCache =
    Map.Map N.Name [V.Version]


initialStore
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m)
    => m Store

initialStore =
  do  versionCache <- readVersionCache
      return (Store Map.empty versionCache)


readVersionCache
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m)
    => m VersionCache

readVersionCache =
  do  cacheDirectory <- asks Manager.cacheDirectory
      let versionsFile = cacheDirectory </> "versions.json"
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

getConstraints
    :: (MonadIO m, MonadReader Manager.Environment m, MonadState Store m, MonadError String m)
    => N.Name
    -> V.Version
    -> m [(N.Name, C.Constraint)]

getConstraints name version =
  do  cache <- gets constraintCache
      case Map.lookup (name, version) cache of
        Just constraints -> return constraints
        Nothing ->
          do  env <- ask
              let path = descriptionPath env name version
              maybeDesc <- decodeFromFile path
              case maybeDesc of
                Just desc ->
                    return (Desc.dependencies desc)
                Nothing ->
                  do  desc <- Http.decodeFromUrl (descriptionUrl env name version)
                      let constraints = Desc.dependencies desc
                      liftIO $ encodeToFile desc path
                      modify $ \store ->
                          store {
                              constraintCache =
                                  Map.insert (name, version) constraints (constraintCache store)
                          }
                      return constraints


descriptionPath :: Manager.Environment -> N.Name -> V.Version -> FilePath
descriptionPath env name version =
    Manager.cacheDirectory env
        </> N.toFilePath name
        </> V.toString version
        </> P.description


descriptionUrl :: Manager.Environment -> N.Name -> V.Version -> FilePath
descriptionUrl env name version =
    Manager.catalog env
        ++ "/catalog"
        ++ "/" ++ N.toFilePath name
        ++ "/" ++ V.toString version
        ++ "/" ++ P.description


-- VERSIONS

getVersions :: (MonadIO m, MonadError String m, MonadState Store m) => N.Name -> m [V.Version]
getVersions name =
  do  cache <- gets versionCache
      case Map.lookup name cache of
        Just versions -> return versions
        Nothing ->
            throwError noLocalVersions
  where
    noLocalVersions =
        unlines
        [ "There are no versions of package '" ++ N.toString name ++ "' on your computer."
        , "Run the following command to update your local package listing:"
        , ""
        , "    elm-package update"
        , ""
        , "Then try to install again."
        ]


-- READ/WRITE TO FILE

decodeFromFile :: (MonadIO m, Json.FromJSON a) => FilePath -> m (Maybe a)
decodeFromFile fullPath =
  do  exists <- liftIO $ Dir.doesFileExist fullPath
      case exists of
        False -> return Nothing
        True ->
          do  fileContents <- liftIO $ BS.readFile fullPath
              return (Json.decode fileContents)


encodeToFile :: Json.ToJSON a => a -> FilePath -> IO ()
encodeToFile value filePath =
  do  Dir.createDirectoryIfMissing True (dropFileName filePath)
      BS.writeFile filePath (Json.encode value)

