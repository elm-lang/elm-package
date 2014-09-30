{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Solution where

import Control.Monad.Error (MonadError, throwError, MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Map as Map
import System.Directory (doesFileExist)

import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V


type Solution =
    Map.Map N.Name V.Version


-- READING AND WRITING SOLUTIONS

writeSolution :: FilePath -> Solution -> IO ()
writeSolution filePath solution =
    BS.writeFile filePath (encodePretty (solutionToJson solution))


readSolutionOr :: (MonadIO m, MonadError String m) => FilePath -> m Solution -> m Solution
readSolutionOr path recover =
  do  exists <- liftIO (doesFileExist path)
      case exists of
        False -> recover
        True ->
           do rawJson <- liftIO (BS.readFile path)
              either throwCorrupted listToSolution (eitherDecode rawJson)
  where
    throwCorrupted _msg =
        throwError $
            "Unable to extract package information from the " ++ path ++
            " file.\nIt may be corrupted."


-- CONVERSION TO JSON

solutionToJson :: Solution -> Value
solutionToJson solution =
    object (map toField (Map.toList solution))
  where
    toField (name, version) =
        Text.pack (N.toString name) .= Text.pack (V.toString version)


listToSolution :: (MonadError String m) => [(String,String)] -> m Solution
listToSolution pairs =
    do  parsedPairs <- mapM parseNameAndVersion pairs
        return (Map.fromList parsedPairs)


parseNameAndVersion :: (MonadError String m) => (String,String) -> m (N.Name, V.Version)
parseNameAndVersion (rawName, rawVersion) =
    do  name <- parse rawName N.fromString ("package name " ++ rawName)
        vrsn <- parse rawVersion V.fromString ("version number for package " ++ rawName)
        return (name, vrsn)


parse :: (MonadError String m) => String -> (String -> Maybe a) -> String -> m a
parse string fromString msg =
    maybe (throwError ("Could not parse " ++ msg)) return (fromString string)


