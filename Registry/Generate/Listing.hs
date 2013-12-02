{-# LANGUAGE OverloadedStrings #-}
module Registry.Generate.Listing where

import Control.Applicative
import Data.Aeson as Json
import Data.Binary as Binary
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS

import qualified Registry.Utils as Utils
import qualified Model.Dependencies as D

add :: D.Deps -> IO ()
add deps =
    do bits <- LBS.readFile Utils.listingBits
       let listings = Binary.decode bits
           name = show $ D.name deps
           version = show $ D.version deps
           insert maybe =
               Just . Listing name (D.summary deps) $
                    case maybe of
                      Nothing -> [version]
                      Just listing -> version : versions listing
           listings' = Map.alter insert name listings
       LBS.length bits `seq` LBS.writeFile Utils.listingBits (Binary.encode listings')
       LBS.writeFile Utils.listing $ Json.encode $ Map.elems listings'

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
