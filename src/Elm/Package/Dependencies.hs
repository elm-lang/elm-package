{-# OPTIONS_GHC -Wall #-}
module Elm.Package.Dependencies where

import Prelude hiding (read)
import Control.Monad (when)
import Control.Monad.Error (runErrorT, throwError, ErrorT, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Map as Map
import System.Directory (doesFileExist)

import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V


type Solution =
    Map.Map N.Name V.Version

type Constraints =
    Map.Map N.Name C.Constraint



{-- CONVERSION TO JSON

toJson :: [(N.Name, V.Version)] -> Value
toJson deps =
    object (map toField deps)
  where
    toField (name, version) =
        Text.pack (N.toString name) .= Text.pack (V.toString version)

fromJson :: Map.Map String String -> ErrorT String IO [(N.Name, V.Version)]
fromJson pairs =
    mapM parseNameAndVersion (Map.toList pairs)


parseNameAndVersion :: (String, String) -> ErrorT String IO (N.Name, V.Version)
parseNameAndVersion (rawName, rawVersion) =
    do  name <- parse rawName N.fromString ("package name " ++ rawName)
        vrsn <- parse rawVersion V.fromString ("version number for package " ++ rawName)
        return (name, vrsn)


parse :: String -> (String -> Maybe a) -> String -> ErrorT String IO a
parse string fromString msg =
    maybe (throwError ("Could not parse " ++ msg)) return (fromString string)


-- READING AND WRITING FILES

write :: FilePath -> [(N.Name, V.Version)] -> IO ()
write filePath pairs =
    BS.writeFile filePath (encodePretty (toJson pairs))

readAnd :: FilePath -> ([(N.Name, V.Version)] -> ErrorT String IO a) -> ErrorT String IO a
readAnd path handle =
  do exists <- liftIO (doesFileExist path)
     when (not exists) throwDoesNotExist
     byteString <- liftIO $ BS.readFile path
     pairs <- either throwCorrupted fromJson (eitherDecode byteString)
     handle pairs
  where
    throwCorrupted _msg =
        throwError $ "Unable to extract package information from the " ++ path ++
                     " file.\nIt may be corrupted."

    throwDoesNotExist =
        throwError ("File " ++ path ++ " does not exist.")


read :: FilePath -> ErrorT String IO [(N.Name, V.Version)]
read path = readAnd path return

readMaybe :: FilePath -> IO (Maybe [(N.Name, V.Version)])
readMaybe path =
  do eitherPairs <- runErrorT (read path)
     case eitherPairs of
       Right pairs -> return (Just pairs)
       Left _ -> return Nothing
-}