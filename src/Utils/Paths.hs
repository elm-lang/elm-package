module Utils.Paths where

import System.FilePath

import qualified Elm.Package as Package

internals = "_internals"

libDir = "public" </> "catalog"

json = "docs.json"
index = "index.elm"
listing = "public" </> "libraries.json"

library name = libDir </> N.toFilePath name

libraryVersion :: Package.Name -> Package.Version -> FilePath
libraryVersion name version =
	library name </> Package.versiontoString version

