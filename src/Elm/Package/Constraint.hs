module Elm.Package.Constraint
    ( Constraint
    , fromString
    , exactly
    , isSatisfied
    ) where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))

import qualified Elm.Package.Version as V


-- ENDPOINTS

data Endpoint
    = Included V.Version
    | Excluded V.Version
    deriving (Show, Eq, Ord)


satisfyUpper :: Endpoint -> V.Version -> Bool
satisfyUpper endpoint version =
    case endpoint of
      Included v -> version <= v
      Excluded v -> version < v


satisfyLower :: Endpoint -> V.Version -> Bool
satisfyLower endpoint version =
    case endpoint of
      Included v -> version >= v
      Excluded v -> version > v


renderUpper :: Endpoint -> String
renderUpper endpoint =
    case endpoint of
      Included v -> "<=" ++ show v
      Excluded v -> "<" ++ show v


renderLower :: Endpoint -> String
renderLower endpoint =
    case endpoint of
      Included v -> ">=" ++ show v
      Excluded v -> ">" ++ show v


-- CONSTRAINTS

data Constraint
    = Range Endpoint Endpoint
    deriving (Show, Eq)


exactly :: V.Version -> Constraint
exactly v =
    Range (Included v) (Included v)


isSatisfied :: Constraint -> V.Version -> Bool
isSatisfied (Range lower upper) version =
    satisfyLower lower version
    && satisfyUpper upper version


parseLower :: String -> Maybe Endpoint
parseLower str =
    case str of
      '>' : '=' : rest -> Included <$> V.fromString rest
      '>' : rest -> Excluded <$> V.fromString rest
      _ -> Nothing


parseUpper :: String -> Maybe Endpoint
parseUpper str =
    case str of
      '<' : '=' : rest -> Included <$> V.fromString rest
      '<' : rest -> Excluded <$> V.fromString rest
      _ -> Nothing


fromString :: String -> Maybe Constraint
fromString str =
    case words str of
      [elem] -> case elem of
        '=' : '=' : rest -> exactly <$> V.fromString rest
        -- second case is written to ease transition from exact versions to constraints,
        -- and should be removed after a couple of releases
        _ -> exactly <$> V.fromString elem
      [elem1, elem2] -> case parseLower elem1 of
        Just ep -> Range ep <$> parseUpper elem2
        Nothing -> (flip Range) <$> parseUpper elem1 <*> parseLower elem2
      _ -> Nothing


instance ToJSON Constraint where
  toJSON = toJSON . toString


toString :: Constraint -> String
toString constr =
    case constr of
      Range (Included v1) (Included v2)
          | v1 == v2 ->
              "==" ++ show v1

      Range lower upper ->
          concat [renderLower lower, " ", renderUpper upper]
