module Get.Install (install) where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_, when)
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory
import System.FilePath

import qualified Get.Utils as Utils
import qualified Get.Registry as Registry
import qualified Model.Dependencies as Deps
import qualified Model.Version as Version

install :: String -> String -> Maybe String -> ErrorT String IO ()
install user library version =
    do location <-
           Utils.inDir Utils.root $ do
             (repo,tag) <- Utils.inDir Utils.internals (get user library version)
             liftIO $ createDirectoryIfMissing True repo
             liftIO $ Utils.copyDir (Utils.internals </> repo) (repo </> tag)
             return (repo </> tag)
       installDependencies (Utils.root </> location </> Utils.depsFile)
    where
      installDependencies :: FilePath -> ErrorT String IO ()
      installDependencies path = do
        exists <- liftIO $ doesFileExist path
        when exists $ do
          deps <- Map.toList <$> Deps.dependencies path
          userLibs <- mapM (Utils.getUserAndProject . fst) deps
          zipWithM_ (\(usr,lib) v -> install usr lib (Just v)) userLibs (map snd deps)

get :: String -> String -> Maybe String -> ErrorT String IO (FilePath, FilePath)
get user library maybeVersion =
  do exists <- liftIO $ doesDirectoryExist directory
     if exists then update else clone
     version <- getVersion repo maybeVersion
     Utils.inDir directory (checkout version)
     return (directory, version)
  where
    directory = user ++ "-" ++ library
    repo = user ++ "/" ++ library

    update = do
      Utils.out $ "Getting updates for repo " ++ repo
      Utils.inDir directory (Utils.git ["pull"])
      return ()

    clone = do
      Utils.out $ "Cloning repo " ++ repo
      Utils.git [ "clone", "--progress", "https://github.com/" ++ repo ++ ".git" ]
      liftIO $ renameDirectory library directory

    checkout version =
        do let tag = show version
           Utils.out $ "Checking out version " ++ tag
           Utils.git [ "checkout", "tags/" ++ tag ]

{-| Check to see that the requested version number exists. In the case that no
version number is requested, use the latest tagless version number in the registry.
If the repo is not in the registry, warn the user and check on github.
-}
getVersion :: String -> Maybe String -> ErrorT String IO String
getVersion repo maybeVersion' =
    do maybeVersion <- validateVersion maybeVersion'
       versions <- getVersions repo
       case maybeVersion of
         Nothing ->
             case filter Version.tagless versions of
               []  -> errorNoTags
               v:_ -> return $ show v
         Just version
             | version `notElem` versions -> errorNoMatch version
             | otherwise                  -> return $ show version
    where
      validateVersion :: Maybe String -> ErrorT String IO (Maybe Version.Version)
      validateVersion version =
          case (version, Version.fromString =<< version) of
            (Just tag, Nothing) ->
                throwError $ unlines $
                [ "tag " ++ tag ++ " is not a valid version number."
                , "It must have the following format: 0.1.2 or 0.1.2-tag"
                ]
            (_, result) -> return result

      getVersions :: String -> ErrorT String IO [Version.Version]
      getVersions repo = do
        registryVersions <- Registry.versions repo
        case registryVersions of
          Just vs -> return vs
          Nothing -> do
            Utils.out $ "Warning: library " ++ repo ++
                        " is not registered publicly. Checking github..."
            tags <- lines <$> Utils.git [ "tag", "--list" ]
            return $ Maybe.mapMaybe Version.fromString tags

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