{-# LANGUAGE OverloadedStrings #-}
module Get.Registry where

import Network
import Network.HTTP
import Network.HTTP.Types
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Resource
import qualified Control.Exception as E

import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List

import qualified Model.Dependencies as D
import qualified Model.Name         as N
import qualified Model.Version      as V
import qualified Get.Utils          as Utils

import Data.Version (showVersion)
import qualified Paths_elm_get as This

domain = "http://localhost:8000"
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
              , partFileSource "deps" Utils.depsFile
              ]

send :: (Manager -> ResourceT IO a) -> ErrorT String IO a
send request =
    do result <- liftIO $ E.catch (Right `fmap` mkRequest) handler
       either throwError return result
    where
      mkRequest = withSocketsDo $ withManager request

      handler exception =
          case exception of
            sce@(StatusCodeException (Status code err) headers _) ->
                let details = case List.lookup "X-Response-Body-Start" headers of
                                Just msg | not (BSC.null msg) -> msg
                                _ -> err
                in  return . Left $ BSC.unpack details

            _ -> return . Left $
                 "probably unable to connect to <" ++ domain ++ "> (" ++
                 show exception ++ ")"