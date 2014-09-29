{-# OPTIONS_GHC -Wall #-}
module Elm.Package.Solution where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError, ErrorT, liftIO)
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


readSolutionOr :: FilePath -> ErrorT String IO Solution -> ErrorT String IO Solution
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


listToSolution :: [(String,String)] -> ErrorT String IO Solution
listToSolution pairs =
    Map.fromList <$> mapM parseNameAndVersion pairs


parseNameAndVersion :: (String,String) -> ErrorT String IO (N.Name, V.Version)
parseNameAndVersion (rawName, rawVersion) =
    do  name <- parse rawName N.fromString ("package name " ++ rawName)
        vrsn <- parse rawVersion V.fromString ("version number for package " ++ rawName)
        return (name, vrsn)


parse :: String -> (String -> Maybe a) -> String -> ErrorT String IO a
parse string fromString msg =
    maybe (throwError ("Could not parse " ++ msg)) return (fromString string)


