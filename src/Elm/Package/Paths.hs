{-# OPTIONS_GHC -Wall #-}
module Elm.Package.Paths where

import System.FilePath ((</>))
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V


{-| Name of directory for all of a project's dependencies. -}
packagesDirectory :: FilePath
packagesDirectory =
	"elm_packages"


{-| Describes the exact versions of every library used for your project. This
information is written by elm-package when it solves and installs dependencies.
-}
solvedDependencies :: FilePath
solvedDependencies =
    packagesDirectory </> "solved-dependencies.json"


{-| Name of the dependency file, specifying dependencies and other metadata
for building and sharing projects.
-}
description :: FilePath
description =
    "elm_package.json"


{-| Path to a particular package. -}
package :: N.Name -> V.Version -> FilePath
package name version =
    packagesDirectory </> N.toFilePath name </> V.toString version