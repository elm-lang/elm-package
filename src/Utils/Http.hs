{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Http where

import qualified Control.Exception as E
import Control.Monad.Error
import Data.Aeson as Json
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Vector as Vector
import Network (withSocketsDo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types

import qualified Elm.Package.Name as Name


send :: (MonadIO m, MonadError String m) => String -> (Request -> Manager -> IO a) -> m a
send url handler =
    do result <- liftIO $ E.catch (Right `fmap` sendRequest) handleError
       either throwError return result
    where
      sendRequest = do
        request <- parseUrl url
        withSocketsDo $ withManager tlsManagerSettings (handler request)

      handleError exception =
          case exception of
            StatusCodeException (Status _code err) headers _ ->
                let details = case List.lookup "X-Response-Body-Start" headers of
                                Just msg | not (BSC.null msg) -> msg
                                _ -> err
                in  return . Left $ BSC.unpack details

            _ -> return . Left $
                 "failed with '" ++ show exception ++ "' when sending request to\n" ++
                 "    <" ++ url ++ ">"


decodeFromUrl :: (MonadIO m, MonadError String m, FromJSON a) => String -> m a
decodeFromUrl url =
  do  result <-
          send url $ \request manager -> do
              response <- httpLbs request manager
              return (decode (responseBody response))

      case result of
        Just v -> return v
        Nothing -> throwError $ "Failure when reading value from " ++ url


-- TAGS from GITHUB

data Tags
    = Tags [String]
    deriving Show


githubTags :: (MonadIO m, MonadError String m) => Name.Name -> m Tags
githubTags name =
    do  response <-
            send url $ \request manager ->
                httpLbs (request {requestHeaders = headers}) manager
        case Json.eitherDecode $ responseBody response of
          Left err -> throwError err
          Right tags -> return tags
    where
      url = "https://api.github.com/repos/" ++ Name.user name ++
            "/" ++ Name.project name ++ "/tags"

      headers = [("User-Agent", "elm-package")] <>
                [("Accept", "application/json")]


instance FromJSON Tags where
    parseJSON (Array arr) = Tags `fmap` mapM toTag list
        where
          list = Vector.toList arr

          toTag (Object obj) = obj .: "name"
          toTag _ = fail "expecting an object"

    parseJSON _ = fail "expecting an array"

