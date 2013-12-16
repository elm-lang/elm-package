module Get.Install (install) where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_, when)
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory
import System.FilePath

import qualified Get.Registry as R
import qualified Utils.Paths as Path
import qualified Utils.Commands as Cmd
import qualified Utils.Http as Http
import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Paths as EPath
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V

install :: N.Name -> Maybe String -> ErrorT String IO ()
install name version =
    do location <-
           Cmd.inDir EPath.dependencyDirectory $ do
             (repo,tag) <- Cmd.inDir Path.internals (get name version)
             liftIO $ createDirectoryIfMissing True repo
             Cmd.copyDir (Path.internals </> repo) (repo </> tag)
             return (repo </> tag)
       let path = EPath.dependencyDirectory </> location </> EPath.dependencyFile
       installDependencies path
    where
      installDependencies :: FilePath -> ErrorT String IO ()
      installDependencies path = do
        exists <- liftIO $ doesFileExist path
        when exists $ do
          deps <- Map.toList <$> D.dependencies path
          names <- mapM (N.fromString' . fst) deps
          zipWithM_ install names (map (Just . snd) deps)

get :: N.Name -> Maybe String -> ErrorT String IO (FilePath, FilePath)
get name maybeVersion =
  do exists <- liftIO $ doesDirectoryExist directory
     if exists then update else clone
     version <- getVersion name maybeVersion
     Cmd.inDir directory (checkout version)
     return (directory, show version)
  where
    directory = N.toFilePath name

    update = do
      Cmd.out $ "Getting updates for repo " ++ show name
      Cmd.inDir directory (Cmd.git ["pull"])
      return ()

    clone = do
      Cmd.out $ "Cloning repo " ++ show name
      Cmd.git [ "clone", "--progress", "https://github.com/" ++ show name ++ ".git" ]
      liftIO $ renameDirectory (N.project name) directory

    checkout version =
        do let tag = show version
           Cmd.out $ "Checking out version " ++ tag
           Cmd.git [ "checkout", "tags/" ++ tag ]

{-| Check to see that the requested version number exists. In the case that no
version number is requested, use the latest tagless version number in the registry.
If the repo is not in the registry, warn the user and check on github.
-}
getVersion :: N.Name -> Maybe String -> ErrorT String IO V.Version
getVersion name maybeVersion' =
    do maybeVersion <- validateVersion maybeVersion'
       versions <- getVersions name
       case maybeVersion of
         Nothing ->
             case filter V.tagless versions of
               [] -> errorNoTags
               vs -> return $ maximum vs
         Just version
             | version `notElem` versions -> errorNoMatch version
             | otherwise                  -> return version
    where
      validateVersion :: Maybe String -> ErrorT String IO (Maybe V.Version)
      validateVersion version =
          case (version, V.fromString =<< version) of
            (Just tag, Nothing) ->
                throwError $ unlines $
                [ "tag " ++ tag ++ " is not a valid version number."
                , "It must have the following format: 0.1.2 or 0.1.2-tag"
                ]
            (_, result) -> return result

      getVersions :: N.Name -> ErrorT String IO [V.Version]
      getVersions name = do
        registryVersions <- R.versions name
        case registryVersions of
          Just vs -> return vs
          Nothing -> do
            Cmd.out $ "Warning: library " ++ show name ++
                      " is not registered publicly. Checking github..."
            tags <- lines <$> Cmd.git [ "tag", "--list" ]
            return $ Maybe.mapMaybe V.fromString tags

      errorNoTags =
          throwError $ unlines 
          [ "did not find any properly tagged releases of this library."
          , "Libraries have at least one tag (like 0.1.2 or 1.0) to ensure that your build"
          , "process is stable and repeatable. These tags should follow Semantic Versioning."
          ]

      errorNoMatch version =
          throwError $ unlines
          [ "could not find version " ++ show version ++ " on github."
          ]