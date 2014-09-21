{-# LANGUAGE OverloadedStrings #-}
module Utils.Http where

import qualified Control.Exception as E
import Control.Monad.Error
import Data.Aeson as Json
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Vector as Vector
import Network
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types

import qualified Elm.Internal.Name as N

send :: String -> (Request -> Manager -> IO a) -> ErrorT String IO a
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

githubTags :: N.Name -> ErrorT String IO Tags
githubTags name =
    do response <- send url $ \request manager ->
                     httpLbs (request {requestHeaders = headers}) manager
       case Json.eitherDecode $ responseBody response of
         Left err -> throwError err
         Right tags -> return tags
    where
      url = "https://api.github.com/repos/" ++ N.user name ++
            "/" ++ N.project name ++ "/tags"

      headers = [("User-Agent", "elm-get")] <>
                [("Accept", "application/json")]


data Tags = Tags [String]
            deriving Show

instance FromJSON Tags where
    parseJSON (Array arr) = Tags `fmap` mapM toTag list
        where
          list = Vector.toList arr

          toTag (Object obj) = obj .: "name"
          toTag _ = fail "expecting an object"

    parseJSON _ = fail "expecting an array"
