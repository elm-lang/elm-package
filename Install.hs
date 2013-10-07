module Install where

import Control.Applicative ((<$>))
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Utils

root = "elm_dependencies"
internals = "_internals"

install :: String -> String -> String -> IO ()
install user library version =
    inDir root $ do
      repo <- inDir internals (get user library version)
      createDirectoryIfMissing True repo
      copyDir (internals </> repo) repo

git = rawSystem "git"

get :: String -> String -> String -> IO FilePath
get user library version = do
  let repo = user ++ "-" ++ library ++ "-" ++ version

  exists <- doesDirectoryExist repo
  case exists of
    False -> do
      git [ "clone", "--progress", "https://github.com/" ++ user ++ "/" ++ library ++ ".git" ]
      renameDirectory library repo
    True -> do
      inDir repo (git ["pull"])
      return ()

  inDir repo (checkout version)
  return repo

checkout :: String -> IO ()
checkout version = do
  git [ "fetch", "--tags" ]
  tags <- lines <$> readProcess "git" [ "tag", "--list" ] ""
  case version `elem` tags of
    False -> do
      putStrLn $ "FAILURE: could not find version " ++ version
      exitFailure
    True -> do
      git [ "checkout", "tags/" ++ version ]
      return ()