{-# LANGUAGE OverloadedStrings #-}
module Publish where

import Control.Monad.Error
import Control.Monad.Reader (ask)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))

import qualified Bump
import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as P
import qualified Elm.Package.Version as V
import qualified Manager
import qualified Utils.Http as Http
import qualified Utils.Paths as Path


publish :: Manager.Manager ()
publish =
  do  description <- Desc.read

      let name = Desc.name description
      let version = Desc.version description

      Cmd.out $ unwords [ "Verifying", N.toString name, V.toString version, "..." ]
      verifyMetadata description
      exposedModules <- locateExposedModules description
      validity <- verifyVersion description
      newVersion <-
          case validity of
            Bump.Valid -> return version
            Bump.Invalid -> throwError "Cannot publish with an invalid version!"
            Bump.Changed v -> return v

      verifyTag name newVersion
      withCleanup $ do
          generateDocs exposedModules
          Catalog.register name newVersion Path.combinedJson
      Cmd.out "Success!"


withCleanup :: Manager.Manager () -> Manager.Manager ()
withCleanup action =
  do  existed <- liftIO $ doesDirectoryExist "docs"
      env <- ask
      either <- liftIO $ Manager.run env action
      when (not existed) $ liftIO $ removeDirectoryRecursive "docs"
      case either of
        Left err -> throwError err
        Right () -> return ()


locateExposedModules :: Desc.Description -> Manager.Manager [FilePath]
locateExposedModules desc =
    mapM locate (Desc.exposed desc)
  where
    locate modul =
      let path = Path.moduleToElmFile modul
          dirs = Desc.sourceDirs desc
      in
      do  possibleLocations <-
              forM dirs $ \dir -> do
                  exists <- liftIO $ doesFileExist (dir </> path)
                  return (if exists then Just (dir </> path) else Nothing)

          case Maybe.catMaybes possibleLocations of
            [] ->
                throwError $
                unlines
                [ "Could not find exposed module '" ++ modul ++ "' when looking through"
                , "the following source directories:"
                , concatMap ("\n    " ++) dirs
                , ""
                , "You may need to add a source directory to your " ++ P.description ++ " file."
                ]

            [location] ->
                return location

            locations ->
                throwError $
                unlines
                [ "I found more than one module named '" ++ modul ++ "' in the"
                , "following locations:"
                , concatMap ("\n    " ++) locations
                , ""
                , "Module names must be unique within your package."
                ]


verifyMetadata :: Desc.Description -> Manager.Manager ()
verifyMetadata deps =
    case problems of
      [] -> return ()
      _  ->
          throwError $
          "Some of the fields in " ++ P.description ++
          " have not been filled in yet:\n\n" ++ unlines problems ++
          "\nFill these in and try to publish again!"
    where
      problems = Maybe.catMaybes
          [ verify Desc.repo        "  repository - must refer to a valid repo on GitHub"
          , verify Desc.summary     "  summary - a quick summary of your project, 80 characters or less"
          , verify Desc.description "  description - extended description, how to get started, any useful references"
          , verify Desc.exposed     "  exposed-modules - list modules your project exposes to users"
          ]

      verify getField msg =
          if getField deps == getField Desc.defaultDescription
            then Just msg
            else Nothing


verifyVersion :: Desc.Description -> Manager.Manager Bump.Validity
verifyVersion description =
  let name = Desc.name description
      version = Desc.version description
  in
  do  maybeVersions <- Catalog.versions name
      case maybeVersions of
        Just publishedVersions ->
            Bump.validateVersion (error "generate docs") name version publishedVersions

        Nothing ->
            Bump.validateInitialVersion description


verifyTag :: N.Name -> V.Version -> Manager.Manager ()
verifyTag name version =
    do  (Http.Tags tags) <- Http.githubTags name
        let publicVersions = Maybe.mapMaybe V.fromString tags
        if version `elem` publicVersions
            then return ()
            else throwError (tagMessage version)


tagMessage :: V.Version -> String
tagMessage version =
    let v = V.toString version in
    unlines
    [ "Libraries must be tagged in git, but tag " ++ v ++ " was not found."
    , "These tags make it possible to find this specific version on github."
    , "To tag the most recent commit and push it to github, run this:"
    , ""
    , "    git tag -a " ++ v ++ " -m \"release version " ++ v ++ "\""
    , "    git push origin " ++ v
    , ""
    ]


generateDocs :: [String] -> Manager.Manager ()
generateDocs modules =
    do  forM elms $ \path -> Cmd.run "elm-doc" [path]
        liftIO $ do
            let path = Path.combinedJson
            BS.writeFile path "[\n"
            let addCommas = List.intersperse (BS.appendFile path ",\n")
            sequence_ $ addCommas $ map append jsons
            BS.appendFile path "\n]"

    where
      elms = map Path.moduleToElmFile modules
      jsons = map Path.moduleToJsonFile modules

      append :: FilePath -> IO ()
      append path = do
        json <- BS.readFile path
        BS.length json `seq` return ()
        BS.appendFile Path.combinedJson json