{-# LANGUAGE OverloadedStrings #-}
module Get.Registry where

import Network (withSocketsDo)
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Resource

import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Model.Dependencies as D
import qualified Model.Name         as N
import qualified Model.Version      as V
{--
import qualified Control.Exception as E
import Control.Exception
import Network.HTTP
import Network.Stream
import qualified Network.URI as URI

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

send :: (Manager -> ResourceT IO a) -> IO a
send request = withSocketsDo $ withManager request

{--
domain = "http://localhost:8000/"

metadata :: N.Name -> ErrorT String IO (Maybe Deps)
metadata name = do
    request GET "metadata" [("library", show name)] $ \body ->
        either throwError return $ Json.eitherDecode body

versions :: N.Name -> ErrorT String IO (Maybe [V.Version])
versions name =
    request GET "versions" [("library", show name)] (return . Binary.decode)

register :: N.Name -> V.Version -> [FilePath] -> ErrorT String IO ()
register library version docs =
    request POST "register" vars $ \x -> let _ = x :: String
                                         in return ()
    where
      vars = [("library", show library), ("version", show version)]

request :: HStream ty => RequestMethod -> String -> [(String,String)] -> (ty -> ErrorT String IO a) -> ErrorT String IO a
request method path vars handler =
    let request = domain ++ path ++ "?" ++ urlEncodeVars vars in
    case URI.parseURI request of
      Nothing  -> throwError $ "could not construct request: " ++ request
      Just uri -> do 
        result <- liftIO $ simpleHTTP (mkRequest method uri) `catch`
                      \err -> let _ = err :: IOException
                              in return $ failMisc $ "Unable to connect to " ++ domain
        case result of
          Left (ErrorMisc str) -> throwError str
          Left err -> throwError (show err)
          Right response
              | rspCode response == (2,0,0) -> handler $ rspBody response
              | otherwise -> throwError $ "request to registry was unsuccessful"
--}