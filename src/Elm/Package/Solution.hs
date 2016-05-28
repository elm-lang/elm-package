{-# OPTIONS_GHC -Wall #-}
module Elm.Package.Solution (Solution, write, read) where

import Prelude hiding (read)
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Elm.Package as Pkg



-- SOLUTION


type Solution =
    Map.Map Pkg.Name Pkg.Version



-- READING AND WRITING SOLUTIONS


write :: FilePath -> Solution -> IO ()
write filePath solution =
  BS.writeFile filePath (encodePretty (toJson solution))


read :: (MonadIO m) => (String -> e) -> FilePath -> ExceptT e m Solution
read toError path =
  do  rawJson <- liftIO (BS.readFile path)
      case eitherDecode rawJson of
        Left err ->
          throwError $ toError err

        Right hashMap ->
          withExceptT toError $ fromJson hashMap



-- TO JSON


toJson :: Solution -> Value
toJson solution =
  let
    toField (name, version) =
      Text.pack (Pkg.toString name) .= Text.pack (Pkg.versionToString version)
  in
    object (map toField (Map.toList solution))



-- FROM JSON


fromJson :: (Monad m) => HashMap.HashMap String String -> ExceptT String m Solution
fromJson hashMap =
  do  pairs <- mapM parseNameAndVersion (HashMap.toList hashMap)
      return (Map.fromList pairs)


parseNameAndVersion :: (Monad m) => (String,String) -> ExceptT String m (Pkg.Name, Pkg.Version)
parseNameAndVersion (rawName, rawVersion) =
  (,)
    <$> parse rawName Pkg.fromString ("package name " ++ rawName)
    <*> parse rawVersion Pkg.versionFromString ("version number for package " ++ rawName)


parse :: (Monad m) => String -> (String -> Either String a) -> String -> ExceptT String m a
parse string fromString msg =
  case fromString string of
    Right a ->
      return a

    Left problem ->
      throwError $ "Could not parse " ++ msg ++ ". " ++ problem