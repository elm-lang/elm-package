module Elm.Package.Constraint
    ( Constraint
    , fromString
    , toString
    , untilNextMajor
    , untilNextMinor
    , expand
    , defaultElmVersion
    , isSatisfied
    , errorMessage
    ) where

import qualified Data.Aeson as Json
import qualified Data.Text as Text

import qualified Elm.Package as Package
import qualified Elm.Compiler as Compiler


-- CONSTRAINTS

data Constraint
    = Range Package.Version Op Op Package.Version


data Op = Less | LessOrEqual


-- CREATE CONSTRAINTS

untilNextMajor :: Package.Version -> Constraint
untilNextMajor version =
  Range version LessOrEqual Less (Package.bumpMajor version)


untilNextMinor :: Package.Version -> Constraint
untilNextMinor version =
  Range version LessOrEqual Less (Package.bumpMinor version)


expand :: Constraint -> Package.Version -> Constraint
expand constraint@(Range lower lowerOp upperOp upper) version
  | version < lower =
      Range version LessOrEqual upperOp upper

  | version > upper =
      Range lower lowerOp Less (Package.bumpMajor version)

  | otherwise =
      constraint


-- ELM CONSTRAINT

defaultElmVersion :: Constraint
defaultElmVersion =
  if Package.major Compiler.version > 0
    then untilNextMajor Compiler.version
    else untilNextMinor Compiler.version


-- CHECK IF SATISFIED

isSatisfied :: Constraint -> Package.Version -> Bool
isSatisfied constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
        isLess lowerOp lower version
          &&
        isLess upperOp version upper


isLess :: (Ord a) => Op -> (a -> a -> Bool)
isLess op =
  case op of
    Less -> (<)
    LessOrEqual -> (<=)


-- STRING CONVERSION

toString :: Constraint -> String
toString constraint =
  case constraint of
    Range lower lowerOp upperOp upper ->
      unwords
        [ Package.versionToString lower
        , opToString lowerOp
        , "v"
        , opToString upperOp
        , Package.versionToString upper
        ]


opToString :: Op -> String
opToString op =
  case op of
    Less -> "<"
    LessOrEqual -> "<="


fromString :: String -> Maybe Constraint
fromString str =
  do  let (lowerString, rest) = break (==' ') str
      lower <- Package.versionFromString lowerString
      (lowerOp, rest1) <- takeOp (eatSpace rest)
      rest2 <- eatV (eatSpace rest1)
      (upperOp, rest3) <- takeOp (eatSpace rest2)
      upper <- Package.versionFromString (eatSpace rest3)
      return (Range lower lowerOp upperOp upper)


eatSpace :: String -> String
eatSpace str =
  case str of
    ' ' : rest -> rest
    _ -> str


takeOp :: String -> Maybe (Op, String)
takeOp str =
  case str of
    '<' : '=' : rest -> Just (LessOrEqual, rest)
    '<' : rest -> Just (Less, rest)
    _ -> Nothing


eatV :: String -> Maybe String
eatV str =
  case str of
    'v' : rest -> Just rest
    _ -> Nothing



-- JSON CONVERSION

instance Json.ToJSON Constraint where
    toJSON constraint =
        Json.toJSON (toString constraint)


instance Json.FromJSON Constraint where
    parseJSON (Json.String text) =
        let rawConstraint = Text.unpack text in
        case fromString rawConstraint of
          Just constraint ->
              return constraint

          Nothing ->
              fail $ errorMessage rawConstraint

    parseJSON _ =
        fail "constraint must be a string that looks something like \"1.2.1 <= v < 2.0.0\"."


errorMessage :: String -> String
errorMessage rawConstraint =
    "Invalid constraint \"" ++ rawConstraint ++ "\"\n\n"
    ++ "    It should look something like \"1.2.1 <= v < 2.0.0\", with no extra or\n"
    ++ "    missing spaces. The middle letter needs to be a 'v' as well.\n\n"
    ++ "    Upper and lower bounds are required so that bounds represent the maximum range\n"
    ++ "    known to work. You do not want to promise users your library will work with\n"
    ++ "    4.0.0 that version has not been tested!"
