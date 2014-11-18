{-# OPTIONS_GHC -Wall #-}
module Elm.Package.Paths where

import System.FilePath ((</>))
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V


{-| Name of directory for all of a project's dependencies. -}
stuffDirectory :: FilePath
stuffDirectory =
    "elm-stuff"


{-| Describes the exact versions of every package used for your project. This
information is written by elm-package when it solves and installs dependencies.
-}
solvedDependencies :: FilePath
solvedDependencies =
    stuffDirectory </> "exact-dependencies.json"


{-| Documentation for all the exposed modules in this package.
-}
documentation :: FilePath
documentation =
    stuffDirectory </> "documentation.json"


{-| Name of the dependency file, specifying dependencies and other metadata
for building and sharing projects.
-}
description :: FilePath
description =
    "elm-package.json"


{-| Directory for all packages needed to build your project.
-}
packagesDirectory :: FilePath
packagesDirectory =
    stuffDirectory </> "packages"


{-| Path to a particular package. -}
package :: N.Name -> V.Version -> FilePath
package name version =
    packagesDirectory </> N.toFilePath name </> V.toString version