{-# LANGUAGE OverloadedStrings #-}
module Catalog where

import Control.Monad.Reader (asks)
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import Data.Version (showVersion)
import Network.HTTP
import Network.HTTP.Client (httpLbs, responseTimeout, responseBody)
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileSource)

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
            response <- httpLbs request manager
            return $ Json.decode $ responseBody response


versions :: N.Name -> Manager.Manager (Maybe [V.Version])
versions name =
    do  url <- catalog "versions" [("library", N.toString name)]
        Http.send url $ \request manager -> do
            response <- httpLbs request manager
            return $ Binary.decode $ responseBody response


register :: N.Name -> V.Version -> FilePath -> Manager.Manager ()
register name version path =
  do  url <- catalog "register" vars
      Http.send url $ \request manager -> do
          request' <- formDataBody files request
          let request'' = request' { responseTimeout = Nothing }
          httpLbs request'' manager
          return ()
  where
    vars =
        [ ("library", N.toString name)
        , ("version", V.toString version)
        ]

    files =
        [ partFileSource "docs" path
        , partFileSource "deps" P.description
        ]
