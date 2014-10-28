{-# LANGUAGE OverloadedStrings #-}
module Catalog where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Version (showVersion)
import Network.HTTP
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Elm.Package.Description as Package
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as P
import qualified Elm.Package.Version as V
import qualified Manager
import qualified Paths_elm_package as This
import qualified Utils.Http as Http


catalog :: String -> [(String,String)] -> Manager.Manager String
catalog path vars =
  do  domain <- asks Manager.catalog
      return $ domain ++ "/" ++ path ++ "?" ++ urlEncodeVars (version : vars)
  where
    version = ("elm-package-version", showVersion This.version)


description :: N.Name -> Manager.Manager (Maybe Package.Description)
description name =
    do  url <- catalog "metadata" [("library", N.toString name)]
        Http.send url $ \request manager -> do
            response <- Client.httpLbs request manager
            return $ Json.decode $ Client.responseBody response


versions :: N.Name -> Manager.Manager (Maybe [V.Version])
versions name =
    do  url <- catalog "versions" [("library", N.toString name)]
        Http.send url $ \request manager -> do
            response <- Client.httpLbs request manager
            return $ Binary.decode $ Client.responseBody response


register :: N.Name -> V.Version -> FilePath -> Manager.Manager ()
register name version path =
  do  url <- catalog "register" vars
      Http.send url $ \request manager -> do
          request' <- Multi.formDataBody files request
          let request'' = request' { Client.responseTimeout = Nothing }
          Client.httpLbs request'' manager
          return ()
  where
    vars =
        [ ("library", N.toString name)
        , ("version", V.toString version)
        ]

    files =
        [ Multi.partFileSource "docs" path
        , Multi.partFileSource "deps" P.description
        ]


docs :: N.Name -> V.Version -> Manager.Manager FilePath
docs name version =
  do  cacheDir <- asks Manager.cacheDirectory
      let path = docsPath cacheDir
      exists <- liftIO (doesFileExist path)
      if exists
          then return path
          else fetchDocs path

  where
    docsPath dir =
        dir </> N.toFilePath name </> V.toString version <.> "json"

    fetchDocs path =
        do  domain <- asks Manager.catalog
            Http.send (docsUrl domain) $ \request manager ->
                do  response <- Client.httpLbs request manager
                    LBS.writeFile path (Client.responseBody response)
                    return path

    docsUrl domain =
        domain ++ "/" ++ N.toString name ++ "/" ++ V.toString version ++ "/docs.json"
