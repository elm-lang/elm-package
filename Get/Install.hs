module Get.Install where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_, when)
import Control.Monad.Error
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import qualified ReadDependencies as Read
import qualified Get.Utils as Utils

root = "elm_dependencies"
internals = "_internals"
depsFile = "elm_dependencies.json"

install :: String -> String -> String -> ErrorT String IO ()
install user library version = do
  location <- Utils.inDir root $ do
                (repo,tag) <- Utils.inDir internals (get user library version)
                liftIO $ createDirectoryIfMissing True repo
                liftIO $ Utils.copyDir (internals </> repo) (repo </> tag)
                return (repo </> tag)
  installDependencies (root </> location </> depsFile)

installDependencies :: FilePath -> ErrorT String IO ()
installDependencies path = do
  exists <- liftIO $ doesFileExist path
  when exists $ do
    deps <- liftIO $ Map.toList <$> Read.dependencies path
    userLibs <- mapM (Utils.getUserAndProject . fst) deps
    zipWithM_ (\(usr,lib) v -> install usr lib v) userLibs (map snd deps)

git :: [String] -> ErrorT String IO String
git args =
  do (exitCode, output) <- liftIO runCommand
     case exitCode of
       ExitSuccess -> return output
       ExitFailure _ -> throwError $ "Error when running: git" ++ concatMap (' ':) args
  where
    runCommand = do
      (_, Just out, Just err, handle) <-
          createProcess (proc "git" args) { std_out = CreatePipe
                                          , std_err = CreatePipe }
      exitCode <- waitForProcess handle
      str <- hGetContents out
      hClose out
      hClose err
      return (exitCode, str)

get :: String -> String -> String -> ErrorT String IO (FilePath,FilePath)
get user library version = do
  let repo = user ++ "-" ++ library
  exists <- liftIO $ doesDirectoryExist repo
  case exists of
    False -> do
      git [ "clone", "--progress", "https://github.com/" ++ user ++ "/" ++ library ++ ".git" ]
      liftIO $ renameDirectory library repo
    True -> do
      Utils.inDir repo (git ["pull"])
      return ()

  Utils.inDir repo (checkout version)
  return (repo,version)

checkout :: String -> ErrorT String IO ()
checkout version =
    do git [ "fetch", "--tags" ]
       tags <- liftIO $ lines <$> readProcess "git" [ "tag", "--list" ] ""
       if version `notElem` tags
         then throwError $ "Did not find version " ++ version ++ " on github."
         else do git [ "checkout", "tags/" ++ version ]
                 return ()

