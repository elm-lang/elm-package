module Publish where

import Control.Monad.Except (throwError)
import qualified Data.Maybe as Maybe

import qualified Bump
import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Package
import qualified Elm.Package.Paths as P
import qualified GitHub
import qualified Manager
import qualified Reporting.Error as Error



publish :: Manager.Manager ()
publish =
  do  description <- Desc.read Error.CorruptDescription P.description

      let name = Desc.name description

      Cmd.out $ unwords $
        [ "Verifying", Package.toString name
        , Package.versionToString (Desc.version description), "..."
        ]

      verifyMetadata description

      docs <- Docs.generate name

      newVersion <- verifyVersion docs description

      verifyTag name newVersion
      Catalog.register name newVersion
      Cmd.out "Success!"



verifyMetadata :: Desc.Description -> Manager.Manager ()
verifyMetadata deps =
    case problems of
      [] ->
        return ()

      _  ->
        throwError $ Error.BadMetadata problems
    where
      problems =
        Maybe.catMaybes
          [ verify Desc.repo "  repository - must refer to a valid repo on GitHub"
          , verify Desc.summary "  summary - a quick summary of your project, 80 characters or less"
          , verify Desc.exposed "  exposed-modules - list modules your project exposes to users"
          ]

      verify getField msg =
        if getField deps == getField Desc.defaultDescription then
          Just msg
        else
          Nothing


verifyVersion
    :: [Docs.Documentation]
    -> Desc.Description
    -> Manager.Manager Package.Version
verifyVersion docs description =
  let
    name =
      Desc.name description

    version =
      Desc.version description
  in
    do  maybeVersions <- Catalog.versions name
        validity <-
          case maybeVersions of
            Just publishedVersions ->
              Bump.validateVersion docs name version publishedVersions

            Nothing ->
              Bump.validateInitialVersion description

        case validity of
          Bump.Valid ->
            return version

          Bump.Invalid ->
            throwError $ Error.InvalidVersion

          Bump.Changed newVersion ->
            return newVersion


verifyTag :: Package.Name -> Package.Version -> Manager.Manager ()
verifyTag name version =
  do  publicVersions <- GitHub.getVersionTags name
      if elem version publicVersions
        then return ()
        else throwError (Error.MissingTag version)