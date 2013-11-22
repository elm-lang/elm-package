{-# LANGUAGE DeriveDataTypeable #-}
module Model.Name where

import           Control.Applicative
import           Data.Aeson
import           Data.Binary
import qualified Data.Text            as T
import           Data.Typeable

data Name = Name { user :: String, project :: String }
    deriving (Typeable,Eq)

instance Binary Name where
  get = Name <$> get <*> get
  put (Name user project) =
      put user >> put project

instance Show Name where
  show name = user name ++ "/" ++ project name

toFilePath :: Name -> FilePath
toFilePath name = user name ++ "-" ++ project name

fromString :: String -> Maybe Name
fromString string =
    case break (=='/') string of
      ( user@(_:_), '/' : project@(_:_) )
          | all (/='/') project -> Just (Name user project)
      _ -> Nothing

instance FromJSON Name where
    parseJSON (String text) =
        let string = T.unpack text in
        case fromString string of
          Just v -> return v
          Nothing -> fail $ unlines
                     [ "Dependency file has an invalid name: " ++ string
                     , "Must have format user/project and match a public github project."
                     ]

    parseJSON _ = fail "Project name must be a string."
