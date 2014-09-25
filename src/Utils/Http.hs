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

import qualified Elm.Package.Name as Name

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

githubTags :: Name.Name -> ErrorT String IO Tags
githubTags name =
    do response <- send url $ \request manager ->
                     httpLbs (request {requestHeaders = headers}) manager
       case Json.eitherDecode $ responseBody response of
         Left err -> throwError err
         Right tags -> return tags
    where
      url = "https://api.github.com/repos/" ++ Name.user name ++
            "/" ++ Name.project name ++ "/tags"

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

-- | Try to read a JSON-encoded value from an URL. Throws an error in case of parse error
decodeFromUrl :: FromJSON a => String -> ErrorT String IO a
decodeFromUrl url =
  do result <- send url $ \request manager ->
       fmap (Json.decode . responseBody) $ httpLbs request manager
     case result of
       Just v -> return v
       Nothing -> throwError $ "Can't read value from " ++ url
