{-# LANGUAGE OverloadedStrings #-}
module GitHub (getVersionTags, publicGetVersionTags) where

import Control.Monad.Except (throwError)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Either as Either
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Network.HTTP.Client

import qualified Elm.Package as Package
import qualified Reporting.Error as Error
import qualified Manager
import qualified Utils.Http as Http



-- TAGS from GITHUB


newtype Tags = Tags [String]


getVersionTags :: Package.Name -> Manager.Manager [Package.Version]
getVersionTags (Package.Name user project) =
  let
    url =
      Text.unpack ("https://api.github.com/repos/" <> user <> "/" <> project <> "/tags")

    headers =
      [ ("User-Agent", "elm-package")
      , ("Accept", "application/json")
      ]
  in
    do  response <-
          Http.send url $ \request manager ->
              httpLbs (request {requestHeaders = headers}) manager

        case Json.eitherDecode (responseBody response) of
          Left err ->
            throwError $ Error.HttpRequestFailed url err

          Right (Tags tags) ->
            return (Either.rights (map Package.versionFromString tags))


instance Json.FromJSON Tags where
  parseJSON json =
    case json of
      Json.Array arr ->
        Tags <$> mapM toTag (Vector.toList arr)

      _ ->
        fail "response is not a JSON array"


toTag :: Json.Value -> Json.Parser String
toTag json =
  case json of
    Json.Object object ->
      object .: "name"

    _ ->
      fail "response is not a JSON array full of objects"



-- PUBLIC VERSION


publicGetVersionTags :: Package.Name -> IO (Either String [Package.Version])
publicGetVersionTags pkg =
  either (Left . Error.toString) Right
    <$> Manager.run (getVersionTags pkg)
