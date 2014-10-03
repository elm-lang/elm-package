{-# LANGUAGE OverloadedStrings #-}
module Diff.Model where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON, parseJSON, (.:), Object, Value(Object))
import Data.Aeson.Types (Parser)
import qualified Data.Map as Map


-- MODEL

data Module = Module
    { adts :: Map.Map String ([String], Map.Map String Type)
    , aliases :: Map.Map String ([String], Type)
    , values :: Map.Map String Type
    }


data Type
    = Var String
    | Type String
    | Lambda Type Type
    | App Type Type
    | Record [(String, Type)] (Maybe Type)


-- FROM JSON

instance FromJSON Module where
    parseJSON (Object obj) =
        Module
            <$> (dict adt =<< obj .: "datatypes")
            <*> (dict alias =<< obj .: "aliases")
            <*> (dict tipe =<< obj .: "values")
        where
            tipe :: Object -> Parser Type
            tipe obj =
                obj .: "type"

            adt :: Object -> Parser ([String], Map.Map String Type)
            adt obj =
                (,) <$> obj .: "typeVariables"
                    <*> (dict tipe =<< obj .: "constructors")

            alias :: Object -> Parser ([String], Type)
            alias obj =
                (,) <$> obj .: "typeVariables"
                    <*> obj .: "type"

    parseJSON _ =
        fail "Module documentation must be represented as an object in JSON"


dict :: (Object -> Parser a) -> [Object] -> Parser (Map.Map String a)
dict getValue objects =
    do  entries <- mapM entry objects
        return (Map.fromList entries)
  where
    entry obj =
        do  name <- obj .: "name"
            value <- getValue obj
            return (name, value)


instance FromJSON Type where
    parseJSON (Object obj) =
        do  tag <- obj .: "tag"
            case (tag :: String) of
                "var" ->
                    Var <$> obj .: "name"

                "function" ->
                    do  args <- obj .: "args"
                        result <- obj .: "result"
                        return (foldr Lambda result args)

                "adt" ->
                    do  name <- obj .: "name"
                        args <- obj .: "args"
                        return (foldl App (Type name) args)

                "record" ->
                    Record
                        <$> obj .: "fields"
                        <*> obj .: "extension"

                _ ->
                    fail $ "Unrecognized tag '" ++ tag ++ "' found."

    parseJSON _ =
        fail "A type must be represented as an object in JSON"
