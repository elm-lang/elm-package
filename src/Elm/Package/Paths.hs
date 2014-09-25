{-# OPTIONS_GHC -Wall #-}
module Elm.Package.Paths where

import System.FilePath ((</>))

{-| Name of directory for all of a project's dependencies. -}
packagesDirectory :: FilePath
packagesDirectory = "elm_packages"

{-| Describes the exact versions of every library used for your project. This
information is written by elm-get when it solves and installs dependencies.
-}
dependencies :: FilePath
dependencies =
    packagesDirectory </> "exact-dependencies.json"

{-| Name of the dependency file, specifying dependencies and other metadata
for building and sharing projects.
-}
description :: FilePath
description =
    "elm_package.json"
