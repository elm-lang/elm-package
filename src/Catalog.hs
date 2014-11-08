{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Catalog where

import Control.Monad.Error (MonadError, throwError)
import Control.Monad.RWS (MonadIO, liftIO, MonadReader, asks)
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as Time
import Data.Version (showVersion)
import Network.HTTP
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>), dropFileName)

import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as P
import qualified Elm.Package.Version as V
import qualified Manager
import qualified Paths_elm_package as This
import qualified Utils.Http as Http


catalog
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m)
    => String
    -> [(String,String)]
    -> m String
catalog path vars =
  do  domain <- asks Manager.catalog
      return $ domain ++ "/" ++ path ++ "?" ++ urlEncodeVars (version : vars)
  where
    version = ("elm-package-version", showVersion This.version)


versions :: N.Name -> Manager.Manager (Maybe [V.Version])
versions name =
  do  url <- catalog "versions" [("name", N.toString name)]
      Http.send url $ \request manager -> do
          response <- Client.httpLbs request manager
          return $ Binary.decode $ Client.responseBody response


allPackages
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m)
    => Maybe Time.UTCTime
    -> m (Maybe [(N.Name, [V.Version])])
allPackages maybeTime =
  do  url <- catalog "all-packages" vars
      Http.send url $ \request manager -> do
          response <- Client.httpLbs request manager
          return $ Binary.decode $ Client.responseBody response
  where
    vars =
      case maybeTime of
        Nothing -> []
        Just time -> [("since", show time)]


register :: N.Name -> V.Version -> Manager.Manager ()
register name version =
  do  url <- catalog "register" vars
      Http.send url $ \request manager -> do
          request' <- Multi.formDataBody files request
          let request'' = request' { Client.responseTimeout = Nothing }
          Client.httpLbs request'' manager
          return ()
  where
    vars =
        [ ("name", N.toString name)
        , ("version", V.toString version)
        ]

    files =
        [ Multi.partFileSource "documentation" P.documentation
        , Multi.partFileSource "description" P.description
        , Multi.partFileSource "readme" "README.md"
        ]


description
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m)
    => N.Name -> V.Version -> m Desc.Description
description name version =
  getJson "description" name version


documentation
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m)
    => N.Name -> V.Version -> m [Docs.Documentation]
documentation name version =
  getJson "documentation" name version


getJson
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m, Json.FromJSON a)
    => String -> N.Name -> V.Version -> m a
getJson metadata name version =
  do  cacheDir <- asks Manager.cacheDirectory
      let metadataPath =
            cacheDir </> N.toFilePath name </> V.toString version </> metadata <.> "json"

      exists <- liftIO (doesFileExist metadataPath)
      
      content <-
        case exists of
          True -> liftIO (LBS.readFile metadataPath)
          False ->
            do  url <- catalog metadata [("name", N.toString name), ("version", V.toString version)]
                Http.send url $ \request manager ->
                    do  response <- Client.httpLbs request manager
                        createDirectoryIfMissing True (dropFileName metadataPath)
                        LBS.writeFile metadataPath (Client.responseBody response)
                        return (Client.responseBody response)
                      
      case Json.eitherDecode content of
        Right value -> return value
        Left err ->
          throwError $
            "Unable to get " ++ metadata ++ " for "
            ++ N.toString name ++ " " ++ V.toString version ++ "\n" ++ err
