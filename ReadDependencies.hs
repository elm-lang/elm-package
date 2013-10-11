{-# LANGUAGE OverloadedStrings #-}
module ReadDependencies where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Exit

data Person = Person
    { name' :: String
    , email :: Maybe String
    , website :: Maybe String
    } deriving Show

data Deps = Deps
    { name :: String
    , version :: Int
    , description :: String
    , license :: String
    , author :: Person
    , contributors :: [Person]
    , repo :: String
    , deps :: Map.Map String String
    } deriving Show

instance FromJSON Person where
    parseJSON (Object v) =
        Person <$> v .: "name"
               <*> v .: "email"
               <*> v .: "website"
    parseJSON (String s) =
        return (Person (Text.unpack s) Nothing Nothing)
    parseJSON _ = mzero

instance FromJSON Deps where
    parseJSON (Object v) =
        Deps <$> v .: "name"
             <*> v .: "version"
             <*> v .: "description"
             <*> v .: "license"
             <*> v .: "author"
             <*> v .: "contributors"
             <*> v .: "repository"
             <*> v .: "dependencies"

    parseJSON _ = mzero

extractDeps :: FilePath -> IO (Map.Map String String)
extractDeps path = do
  json <- BS.readFile path
  case eitherDecode json of
    Right ds -> return (deps ds)
    Left err ->
        do putStrLn $ "Error reading file " ++ path ++ ":\n    " ++ err
           exitFailure
