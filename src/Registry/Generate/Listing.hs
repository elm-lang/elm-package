{-# LANGUAGE OverloadedStrings #-}
module Registry.Generate.Listing where

import Control.Applicative
import Control.Monad (when)
import Data.Aeson as Json
import Data.Binary as Binary
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Utils.Paths as Path
import qualified Elm.Internal.Dependencies as D

add :: D.Deps -> IO ()
add deps =
    do listings <- readListings
       let name = show $ D.name deps
           version = show $ D.version deps
           insert maybe =
               Just . Listing name (D.summary deps) $
                    case maybe of
                      Nothing -> [version]
                      Just listing -> version : versions listing
           listings' = Map.alter insert name listings
       LBS.writeFile Path.listingBits (Binary.encode listings')
       LBS.writeFile Path.listing $ Json.encode $ Map.elems listings'

readListings :: IO (Map.Map String Listing)
readListings =
    do exists <- Dir.doesFileExist Path.listingBits
       when (not exists) $
            let empty = Binary.encode (Map.empty :: Map.Map String [String])
            in  LBS.writeFile Path.listingBits empty
       withBinaryFile Path.listingBits ReadMode $ \handle -> do
         bits <- LBS.hGetContents handle
         LBS.length bits `seq` return (Binary.decode bits)


data Listing = Listing
    { name :: String
    , summary :: String
    , versions :: [String]
    }

instance Binary Listing where
    get = Listing <$> get <*> get <*> get
    put (Listing name summary versions) =
        put name >> put summary >> put versions

instance ToJSON Listing where
    toJSON (Listing name summary versions) =
        object ["name" .= name, "summary" .= summary, "versions" .= versions]
