{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub (getVersionTags) where

import Control.Monad.Error
import Data.Aeson as Json
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import qualified Data.Vector as Vector
import Network.HTTP.Client

import qualified Elm.Package.Name as Name
import qualified Elm.Package.Version as Version
import qualified Utils.Http as Http


-- TAGS from GITHUB

newtype Tags = Tags [String]


getVersionTags
    :: (MonadIO m, MonadError String m)
    => Name.Name -> m [Version.Version]

getVersionTags (Name.Name user project) =
  do  response <-
          Http.send url $ \request manager ->
              httpLbs (request {requestHeaders = headers}) manager
      case Json.eitherDecode (responseBody response) of
        Left err -> throwError err
        Right (Tags tags) -> return (Maybe.mapMaybe Version.fromString tags)

  where
    url = "https://api.github.com/repos/" ++ user ++ "/" ++ project ++ "/tags"

    headers =
        [("User-Agent", "elm-package")]
        <> [("Accept", "application/json")]


instance FromJSON Tags where
    parseJSON (Array arr) =
        Tags `fmap` mapM toTag (Vector.toList arr)
      where
        toTag (Object obj) = obj .: "name"
        toTag _ = fail "expecting an object"

    parseJSON _ = fail "expecting an array"

