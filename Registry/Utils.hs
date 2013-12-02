module Registry.Utils where

import System.FilePath
import qualified Model.Name as N
import qualified Model.Version as V

libDir = "public" </> "catalog"

json = "docs.json"
index = "index.elm"
listing = "public" </> "libraries.json"
listingBits = "listing.bits"

library name = libDir </> N.toFilePath name

libraryVersion :: N.Name -> V.Version -> FilePath
libraryVersion name version = library name </> show version