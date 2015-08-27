{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Solution (Solution, write, read) where

import Prelude hiding (read)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Elm.Package as Package


type Solution =
    Map.Map Package.Name Package.Version


-- READING AND WRITING SOLUTIONS

write :: FilePath -> Solution -> IO ()
write filePath solution =
    BS.writeFile filePath (encodePretty (toJson solution))


read :: (MonadIO m, MonadError String m) => FilePath -> m Solution
read path =
  do  rawJson <- liftIO (BS.readFile path)
      either throwCorrupted fromJson (eitherDecode rawJson)
  where
    throwCorrupted _msg =
        throwError $
            "Unable to extract package information from the " ++ path ++
            " file.\nIt may be corrupted."


-- CONVERSION TO JSON

toJson :: Solution -> Value
toJson solution =
    object (map toField (Map.toList solution))
  where
    toField (name, version) =
        Text.pack (Package.toString name) .= Text.pack (Package.versionToString version)


fromJson :: (MonadError String m) => HashMap.HashMap String String -> m Solution
fromJson hashMap =
    do  pairs <- mapM parseNameAndVersion (HashMap.toList hashMap)
        return (Map.fromList pairs)


parseNameAndVersion :: (MonadError String m) => (String,String) -> m (Package.Name, Package.Version)
parseNameAndVersion (rawName, rawVersion) =
    do  name <- parse rawName Package.fromString ("package name " ++ rawName)
        vrsn <- parse rawVersion Package.versionFromString ("version number for package " ++ rawName)
        return (name, vrsn)


parse :: (MonadError String m) => String -> (String -> Maybe a) -> String -> m a
parse string fromString msg =
    maybe (throwError ("Could not parse " ++ msg)) return (fromString string)
