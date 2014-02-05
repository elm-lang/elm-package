{-# LANGUAGE OverloadedStrings #-}
module Get.Registry where

import Network.HTTP
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import Data.Version (showVersion)

import qualified Paths_elm_get as This
import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Name         as N
import qualified Elm.Internal.Version      as V
import qualified Elm.Internal.Paths        as Path
import qualified Utils.Http                as Http

domain = "http://library.elm-lang.org"

libraryUrl path vars =
    domain ++ "/" ++ path ++ "?" ++ urlEncodeVars (version : vars)
  where
    version = ("elm-get-version", showVersion This.version)

metadata :: N.Name -> ErrorT String IO (Maybe D.Deps)
metadata name =
    Http.send domain $ \manager ->
    do request  <- parseUrl $ libraryUrl "metadata" [("library", show name)]
       response <- httpLbs request manager
       return $ Json.decode $ responseBody response

versions :: N.Name -> ErrorT String IO (Maybe [V.Version])
versions name =
    Http.send domain $ \manager ->
    do request  <- parseUrl $ libraryUrl "versions" [("library", show name)]
       response <- httpLbs request manager
       return $ Binary.decode $ responseBody response

register :: N.Name -> V.Version -> FilePath -> ErrorT String IO ()
register name version path =
    Http.send domain $ \manager ->
    do request <- parseUrl $ libraryUrl "register" vars
       request' <- formDataBody files request
       let request'' = request' { responseTimeout = Nothing }
       httpLbs request'' manager
       return ()
    where
      vars = [ ("library", show name), ("version", show version) ]
      files = [ partFileSource "docs" path
              , partFileSource "deps" Path.dependencyFile
              ]
