{-# LANGUAGE OverloadedStrings #-}
module Utils.Http where

import Network
import Network.HTTP.Types
import Network.HTTP.Client

import Control.Monad.Error
import qualified Control.Exception as E

import Data.Aeson as Json
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as BSC
import qualified Elm.Internal.Name as N

send :: String -> (Manager -> IO a) -> ErrorT String IO a
send domain request =
    do result <- liftIO $ E.catch (Right `fmap` mkRequest) handler
       either throwError return result
    where
      mkRequest = withSocketsDo $ withManager defaultManagerSettings request

      handler exception =
          case exception of
            StatusCodeException (Status _code err) headers _ ->
                let details = case List.lookup "X-Response-Body-Start" headers of
                                Just msg | not (BSC.null msg) -> msg
                                _ -> err
                in  return . Left $ BSC.unpack details

            _ -> return . Left $
                 "probably unable to connect to <" ++ domain ++ "> (" ++
                 show exception ++ ")"

githubTags :: N.Name -> ErrorT String IO Tags
githubTags name =
    do response <- send "https://api.github.com" $ \manager -> do
                     request <- parseUrl url
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
