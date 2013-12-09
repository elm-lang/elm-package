{-# LANGUAGE OverloadedStrings #-}
module Get.Registry where

import Network.HTTP
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

import Control.Monad.Trans.Resource
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary

import Data.Version (showVersion)
import qualified Paths_elm_get as This
import qualified Utils.Model.Dependencies as D
import qualified Utils.Model.Name         as N
import qualified Utils.Model.Version      as V
import Utils.Paths (depsFile)

domain = "http://library.elm-lang.org"
url path vars =
    domain ++ "/" ++ path ++ "?" ++ urlEncodeVars (version : vars)
  where
    version = ("elm-get-version", showVersion This.version)

metadata :: N.Name -> Manager -> ResourceT IO (Maybe D.Deps)
metadata name manager =
    do request  <- parseUrl $ url "metadata" [("library", show name)]
       response <- httpLbs request manager
       return $ Json.decode $ responseBody response

versions :: N.Name -> Manager -> ResourceT IO (Maybe [V.Version])
versions name manager =
    do request  <- parseUrl $ url "versions" [("library", show name)]
       response <- httpLbs request manager
       return $ Binary.decode $ responseBody response

register :: N.Name -> V.Version -> FilePath -> Manager -> ResourceT IO ()
register name version path manager =
    do request <- parseUrl $ url "register" vars
       request' <- formDataBody files request
       httpLbs request' manager
       return ()
    where
      vars = [ ("library", show name), ("version", show version) ]
      files = [ partFileSource "docs" path
              , partFileSource "deps" depsFile
              ]
