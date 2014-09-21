{-# LANGUAGE OverloadedStrings #-}
module Generate.Listing where

import Control.Applicative
import Control.Monad (when)
import Data.Aeson as Json
import Data.Aeson.Encode.Pretty as Json
import qualified Data.List as List
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import qualified Utils.Paths as Path

jsonEncode :: ToJSON a => a -> LBS.ByteString
jsonEncode = Json.encodePretty' (defConfig { confCompare = keyOrder order })
    where
      order = ["name","summary","versions"]

add :: D.Deps -> IO ()
add deps =
    do listings <- readListings
       let listings' =
             if any (\lstng -> name lstng == project) listings
               then map cons listings
               else List.insert listing listings
       LBS.writeFile Path.listing (jsonEncode listings')
    where
      project = D.name deps
      version = D.version deps
      listing = Listing project (D.summary deps) [version]

      cons lstng@(Listing project' _ _)
          | project == project' = lstng { versions = version : versions lstng }
          | otherwise           = lstng

readListings :: IO [Listing]
readListings =
    do exists <- Dir.doesFileExist Path.listing
       when (not exists) $
            LBS.writeFile Path.listing (jsonEncode ([] :: [Listing]))
       withBinaryFile Path.listing ReadMode $ \handle -> do
         json <- LBS.hGetContents handle
         LBS.length json `seq` return (maybe [] id (Json.decode json))

data Listing = Listing
    { name     :: N.Name
    , summary  :: String
    , versions :: [V.Version]
    } deriving (Show,Eq,Ord)

instance ToJSON Listing where
    toJSON (Listing name summary versions) =
        object ["name" .= name, "summary" .= summary, "versions" .= versions]

instance FromJSON Listing where
    parseJSON json =
        case json of
          Object v ->
              Listing <$> v .: "name"
                      <*> v .: "summary"
                      <*> v .: "versions"
          _ -> error "Cannot convert non-objects into listings."
