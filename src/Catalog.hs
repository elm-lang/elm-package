{-# LANGUAGE OverloadedStrings #-}
module Catalog where

import Control.Monad.Except (throwError)
import Control.Monad.RWS (liftIO, asks)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as Time
import Data.Version (showVersion)
import Network.HTTP
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), dropFileName)

import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Package
import qualified Elm.Package.Paths as P
import qualified Manager
import qualified Paths_elm_package as This
import qualified Reporting.Error as Error
import qualified Utils.Http as Http



-- MAKE URL


catalog :: String -> [(String,String)] -> Manager.Manager String
catalog path vars =
  do  domain <- asks Manager.catalog
      return $ domain ++ "/" ++ path ++ "?" ++ urlEncodeVars (version : vars)
  where
    version = ("elm-package-version", showVersion This.version)



-- EASY REQUESTS


versions :: Package.Name -> Manager.Manager (Maybe [Package.Version])
versions name =
  get "versions" [("name", Package.toString name)]


permissions :: Package.Name -> Manager.Manager Bool
permissions name =
  get "permissions" [("name", Package.toString name)]


get :: (Binary.Binary a) => String -> [(String,String)] -> Manager.Manager a
get path vars =
  do  url <- catalog path vars
      Http.send url $ \request manager ->
        do  response <- Client.httpLbs request manager
            return $ Binary.decode $ Client.responseBody response



-- FANCIER REQUESTS


allPackages :: Maybe Time.UTCTime -> Manager.Manager (Maybe [(Package.Name, [Package.Version])])
allPackages maybeTime =
  do  url <- catalog "all-packages" vars
      Http.send url $ \request manager -> do
          response <- Client.httpLbs request manager
          case Json.eitherDecode (Client.responseBody response) of
            Left _ ->
              return Nothing

            Right summaries ->
              return $ Just $ map (\(PackageSummary s) -> s) summaries
  where
    vars =
      case maybeTime of
        Nothing -> []
        Just time -> [("since", show time)]


newtype PackageSummary = PackageSummary (Package.Name, [Package.Version])


instance Json.FromJSON PackageSummary where
    parseJSON (Json.Object obj) =
      do  name <- obj .: "name"
          versions <- obj .: "versions"
          return (PackageSummary (name, versions))

    parseJSON _ =
      fail "package summary must be an object"


register :: Package.Name -> Package.Version -> Manager.Manager ()
register name version =
  do  url <- catalog "register" vars
      Http.send url $ \request manager -> do
          request' <- Multi.formDataBody files request
          let request'' = request' { Client.responseTimeout = Nothing }
          Client.httpLbs request'' manager
          return ()
  where
    vars =
        [ ("name", Package.toString name)
        , ("version", Package.versionToString version)
        ]

    files =
        [ Multi.partFileSource "documentation" P.documentation
        , Multi.partFileSource "description" P.description
        , Multi.partFileSource "readme" "README.md"
        ]



-- GET JSON


description :: Package.Name -> Package.Version -> Manager.Manager Desc.Description
description name version =
  getJson "description" P.description name version


documentation :: Package.Name -> Package.Version -> Manager.Manager [Docs.Documentation]
documentation name version =
  getJson "documentation" "documentation.json" name version


getJson :: (Json.FromJSON a) => String -> FilePath -> Package.Name -> Package.Version -> Manager.Manager a
getJson metadata metadataPath name version =
  do  cacheDir <- asks Manager.cacheDirectory
      let fullMetadataPath =
            cacheDir </> Package.toFilePath name </> Package.versionToString version </> metadataPath

      exists <- liftIO (doesFileExist fullMetadataPath)

      content <-
        case exists of
          True ->
            liftIO (LBS.readFile fullMetadataPath)

          False ->
            do  url <- catalog metadata [("name", Package.toString name), ("version", Package.versionToString version)]
                Http.send url $ \request manager ->
                    do  response <- Client.httpLbs request manager
                        createDirectoryIfMissing True (dropFileName fullMetadataPath)
                        LBS.writeFile fullMetadataPath (Client.responseBody response)
                        return (Client.responseBody response)

      case Json.eitherDecode content of
        Right value ->
          return value

        Left _ ->
          throwError $ Error.CorruptJson metadataPath name version
