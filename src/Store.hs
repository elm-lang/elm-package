{-# LANGUAGE FlexibleContexts #-}
module Store (Store, getConstraints, getVersions, initialStore) where

import Control.Monad.Error (MonadError, throwError)
import Control.Monad.RWS (MonadIO, liftIO, MonadReader, ask, MonadState, gets, modify)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>), dropFileName)

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


initialStore :: (MonadIO m, MonadError String m) => Manager.Environment -> [(N.Name, C.Constraint)] -> m Store
initialStore env localConstraints =
  do  let cacheDirectory = Manager.cacheDirectory env
      maybeVersions <- decodeFromFile (cacheDirectory </> "versions.json")
      case maybeVersions of
        Nothing ->
            throwError noLocalPackages
        Just versions ->
            let constraintCache = Map.singleton (N.dummyName, V.dummyVersion) localConstraints
                versionCache = Map.fromList (versions :: [(N.Name, [V.Version])])
            in
                return (Store constraintCache versionCache)
  where
    noLocalPackages =
        unlines
        [ "Before installing anything, you need to run the following command to"
        , "update your local package listing:"
        , ""
        , "    elm-package update"
        , ""
        , "This will download all of the latest packages available."
        , "After that, try to install again."
        ]


-- CONSTRAINTS

getConstraints
    :: (MonadIO m, MonadReader Manager.Environment m, MonadState Store m, MonadError String m)
    => N.Name -> V.Version -> m [(N.Name, C.Constraint)]
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

