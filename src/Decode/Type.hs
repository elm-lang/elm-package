{-# LANGUAGE OverloadedStrings #-}
module Decode.Type where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Text as Text
import Data.Traversable (traverse)
import qualified Data.Vector as Vector

data Type = App Type Type
          | Var Text.Text
          | Record Type (HM.HashMap Text.Text Type)
          | EmptyRecord
            deriving (Show,Eq)

instance FromJSON Type where
  parseJSON value =
    case value of
      Object pairs ->
          if HM.null pairs then return EmptyRecord else
          do rawFields <- traverse parseJSON pairs
             let ext = HM.lookupDefault EmptyRecord "_" rawFields
             return $ Record ext (HM.delete "_" rawFields)

      Array values ->
          case Vector.length values of
            0 -> mzero
            1 -> parseJSON (Vector.head values)
            _ -> do types <- traverse parseJSON values
                    return $ Vector.foldl1 App types

      String s -> return $ Var s

      _ -> mzero
