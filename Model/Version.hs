{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Model.Version where

import           Control.Applicative
import           Data.Binary
import           Data.Char              (isDigit)
import qualified Data.List              as List
import qualified Data.SafeCopy          as SC
import           Data.Typeable

-- Data representation

data Version = V [Int] String
    deriving (Typeable,Eq)

$(SC.deriveSafeCopy 0 'SC.base ''Version)

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

instance Binary Version where
  get = V <$> get <*> get
  put (V ns tag) = do put ns
                      put tag

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

