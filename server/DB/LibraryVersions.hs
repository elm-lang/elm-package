{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module DB.LibraryVersions (open, register, versions) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import qualified Data.SafeCopy as SC
import Data.Typeable
import Data.Char (isDigit)
import qualified Data.Map as Map
import qualified Data.List as List

-- Data representation

type Library = String
data Version = V [Int] String
    deriving (Typeable,Eq)

instance Ord Version where
  compare (V ns tag) (V ns' tag') =
      case compare ns ns' of
        EQ -> reverseOrder tag tag'
        cmp -> cmp

reverseOrder v1 v2 =
    case compare v1 v2 of { LT -> GT ; EQ -> EQ ; GT -> LT }

instance Show Version where
  show (V ns tag) =
      List.intercalate "." (map show ns) ++ if null tag then "" else "-" ++ tag

tagless :: Version -> Bool
tagless (V _ tag) = null tag

fromString :: String -> Maybe Version
fromString version = V <$> splitNumbers possibleNumbers <*> tag
    where
      (possibleNumbers, possibleTag) = break ((==) '-') version

      tag = case possibleTag of
              "" -> Just ""
              '-':rest -> Just rest
              _ -> Nothing

      parse :: String -> Maybe Int
      parse number
          | null number || any (not . isDigit) number = Nothing
          | otherwise = Just (read number)

      splitNumbers :: String -> Maybe [Int]
      splitNumbers ns =
          case break ((==) '.') ns of
            (number, []) -> (:[]) <$> parse number
            (number, '.':rest) -> (:) <$> parse number <*> splitNumbers rest
            _ -> Nothing

data LibraryVersions = LibraryVersions !(Map.Map Library [Version])
    deriving (Typeable)

$(SC.deriveSafeCopy 0 'SC.base ''Version)
$(SC.deriveSafeCopy 0 'SC.base ''LibraryVersions)


-- Transactions

acidRegister :: Library -> Version -> Update LibraryVersions ()
acidRegister library version =
    do LibraryVersions m <- get
       put (LibraryVersions (Map.insertWith (\[v] -> List.insertBy reverseOrder v) library [version] m))

acidVersions :: Library -> Query LibraryVersions (Maybe [Version])
acidVersions library =
    do LibraryVersions m <- ask
       return (Map.lookup library m)

$(makeAcidic ''LibraryVersions ['acidRegister, 'acidVersions])

type DB = AcidState LibraryVersions

register :: DB -> Library -> String -> ErrorT String IO ()
register db library rawVersion =
    case fromString rawVersion of
      Just version -> do
        liftIO $ update db (AcidRegister library version)
        return ()
      Nothing -> throwError $ unlines
                 [ "Could not register: " ++ rawVersion ++ " is not a valid version."
                 , "Versions must have one of the following formats: 0.1.2 or 0.1.2-tag"
                 ]

rawVersions :: DB -> Library -> ErrorT String IO [Version]
rawVersions db library =
    do maybe <- liftIO $ query db (AcidVersions library)
       case maybe of
         Nothing -> throwError $ "Could not find a library named " ++ library ++ "!"
         Just versions -> return versions

versions :: DB -> Library -> ErrorT String IO [String]
versions db lib = map show <$> rawVersions db lib

latestUntagged :: DB -> Library -> ErrorT String IO String
latestUntagged db library =
    do vs <- rawVersions db library
       case filter tagless vs of
         v:_ -> return (show v)
         [] -> throwError $ unlines
               [ "There is no untagged release of " ++ library
               , "Try using one of the tagged releases: " ++ 
                 List.intercalate ", " (map show vs)
               ]


-- Open

open :: IO DB
open = openLocalState (LibraryVersions Map.empty)