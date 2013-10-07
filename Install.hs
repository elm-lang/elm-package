module Install where

import Control.Applicative ((<$>))
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Utils

root = "elm_dependencies"
internals = "_internals"

install :: String -> String -> Maybe String -> IO ()
install user library version =
    inDir root $ do
      (repo,tag) <- inDir internals (get user library version)
      createDirectoryIfMissing True repo
      copyDir (internals </> repo) (repo </> tag)

git = rawSystem "git"

get :: String -> String -> Maybe String -> IO (FilePath,FilePath)
get user library version = do
  let repo = user ++ "-" ++ library
  exists <- doesDirectoryExist repo
  case exists of
    False -> do
      git [ "clone", "--progress", "https://github.com/" ++ user ++ "/" ++ library ++ ".git" ]
      renameDirectory library repo
    True -> do
      inDir repo (git ["pull"])
      return ()

  tag <- inDir repo (checkout version)
  return (repo,tag)

checkout :: Maybe String -> IO String
checkout version = do
  git [ "fetch", "--tags" ]
  tags <- lines <$> readProcess "git" [ "tag", "--list" ] ""
  case getTag version tags of
    Just tag | tag `elem` tags -> do
      git [ "checkout", "tags/" ++ tag ]
      return tag
    _ -> do
      let msg = Maybe.maybe "a valid version" (\v -> "version " ++ show v) version
      putStrLn $ "FAILURE: could not find " ++ msg
      exitFailure

getTag version tags =
    case version of
      Just _ -> version
      _ -> case List.sort $ Maybe.mapMaybe validVersion tags of
             [] -> Nothing
             vs -> Just . List.intercalate "." . map show $ last vs

validVersion :: String -> Maybe [Int]
validVersion tag = 
    case span Char.isDigit tag of
      ("", _:_) -> Nothing
      (n, '.' : rest) -> (:) (read n) <$> validVersion rest
      (n, "") -> Just [read n]
      _ -> Nothing
