{-# LANGUAGE OverloadedStrings #-}
module Model.Dependencies where

import Control.Applicative
import Control.Monad.Error
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Model.Name as N
import qualified Model.Version as V

data Person = Person
    { name' :: String
    , email :: Maybe String
    , website :: Maybe String
    } deriving Show

data Deps = Deps
    { name :: N.Name
    , version :: V.Version
    , summary :: String
    , description :: String
    , tags :: [String]
    , license :: String
    , author :: Person
    , contributors :: [Person]
    , repo :: String
    , exposed :: [String]
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
             <*> do summary <- v .: "summary"
                    if length summary < 80 then return summary else
                        fail "'summary' must be less than 80 characters"
             <*> v .: "description"
             <*> v .: "tags"
             <*> v .: "license"
             <*> v .: "author"
             <*> v .: "contributors"
             <*> v .: "repository"
             <*> v .: "exposed-modules"
             <*> v .: "dependencies"

    parseJSON _ = mzero

withDeps :: (Deps -> a) -> FilePath -> ErrorT String IO a
withDeps handle path =
    do json <- readPath
       case eitherDecode json of
         Left err -> throwError $ "Error reading file " ++ path ++ ":\n    " ++ err
         Right ds -> return (handle ds)
    where
      readPath :: ErrorT String IO BS.ByteString
      readPath = do
        result <- liftIO $ catch (Right <$> BS.readFile path)
                                 (\err -> return $ Left (err :: IOError))
        case result of
          Right bytes -> return bytes
          Left _ -> throwError $
                    "could not find file " ++ path ++
                    "\n    You may need to create a dependency file for your project."

dependencies :: FilePath -> ErrorT String IO (Map.Map String String)
dependencies = withDeps deps

depsAt :: FilePath -> ErrorT String IO Deps
depsAt = withDeps id