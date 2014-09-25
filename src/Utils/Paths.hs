module Utils.Paths where

import System.FilePath

import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V

internals = "_internals"

libDir = "public" </> "catalog"

json = "docs.json"
index = "index.elm"
listing = "public" </> "libraries.json"

library name = libDir </> N.toFilePath name

libraryVersion :: N.Name -> V.Version -> FilePath
libraryVersion name version = library name </> show version

moduleToElmFile :: String -> FilePath
moduleToElmFile moduleName = swapDots moduleName ++ ".elm"

moduleToJsonFile :: String -> FilePath
moduleToJsonFile moduleName = "docs" </> swapDots moduleName ++ ".json"

swapDots :: String -> String
swapDots = map (\c -> if c == '.' then '/' else c)

combinedJson :: FilePath
combinedJson = "docs" </> "docs.json"
