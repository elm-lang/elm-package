{-# LANGUAGE OverloadedStrings #-}
module Get.Registry where

import Network
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Resource

import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List

import qualified Model.Dependencies as D
import qualified Model.Name         as N
import qualified Model.Version      as V

import qualified Control.Exception as E
import Network.HTTP.Types
{--
import Data.Version (showVersion)
import qualified Paths_elm_get as This
--}

domain = "http://localhost:8000/"

toBS :: Show t => t -> BSC.ByteString
toBS = BSC.pack . show

metadata :: N.Name -> Manager -> ResourceT IO (Maybe D.Deps)
metadata name manager =
    do request  <- parseUrl $ domain ++ "metadata"
       request' <- formDataBody vars request
       response <- httpLbs request' manager
       return $ Json.decode $ responseBody response
    where
      vars = [ partBS "library" (toBS name) ]

versions :: N.Name -> Manager -> ResourceT IO (Maybe [V.Version])
versions name manager =
    do request  <- parseUrl $ domain ++ "versions"
       request' <- formDataBody vars request
       response <- httpLbs request' manager
       return $ Binary.decode $ responseBody response
    where
      vars = [ partBS "library" (toBS name) ]

register :: N.Name -> V.Version -> FilePath -> Manager -> ResourceT IO ()
register name version path manager =
    do request <- parseUrl $ domain ++ "register"
       request' <- formDataBody vars request
       httpLbs request' manager
       return ()
    where
      vars = [ partBS "library" (toBS name)
             , partBS "version" (toBS version)
             , partFileSource "docs" path
             ]

send :: (Manager -> ResourceT IO a) -> ErrorT String IO a
send request =
    do result <- liftIO $ E.catch (Right `fmap` mkRequest) handler
       either throwError return result
    where
      mkRequest = withSocketsDo $ withManager request

      handler (StatusCodeException (Status code err) headers _) =
          return . Left $ BSC.unpack err ++ details
          where
            details = case List.lookup "X-Response-Body-Start" headers of
                        Nothing -> ""
                        Just msg -> ": " ++ BSC.unpack msg
