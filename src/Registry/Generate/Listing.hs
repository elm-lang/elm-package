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
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V

add :: D.Deps -> IO ()
add deps =
    do listings <- readListings
       let name = D.name deps
           version = D.version deps
           insert maybe =
               Just . Listing name (D.summary deps) $
                    case maybe of
                      Nothing -> [version]
                      Just listing -> version : versions listing
           listings' = Map.alter insert (show name) listings
       LBS.writeFile Path.listingBits (Binary.encode listings')
       LBS.writeFile Path.listing $ Json.encode $ Map.elems listings'

remove :: String -> IO ()
remove name =
    do listings <- readListings
       let listings' = Map.delete name listings
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
    { name :: N.Name
    , summary :: String
    , versions :: [V.Version]
    }

instance Binary Listing where
    get = Listing <$> get <*> get <*> get
    put (Listing name summary versions) =
        put name >> put summary >> put versions

instance ToJSON Listing where
    toJSON (Listing name summary versions) =
        object ["name" .= name, "summary" .= summary, "versions" .= versions]
