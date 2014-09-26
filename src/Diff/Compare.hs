{-# LANGUAGE OverloadedStrings #-}
module Diff.Compare where

import Control.Monad.Error
import Data.Aeson hiding (Number)
import Data.Aeson.Types (Parser)
import Data.Functor ((<$>))
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector

import qualified Elm.Package.Version as V

-- DATATYPE DEFINITIONS

data Magnitude
    = Major
    | Minor
    | Patch


data Compatibility
    = Incompatible
    | Compatible
    | Same


magnitude :: Compatibility -> Magnitude
magnitude compat =
  case compat of
    Incompatible -> Major
    Compatible -> Minor
    Same -> Patch


data DiffError
    = ParsingError
    | StringError String

data Difference
    = DifferentRenaming Var Var Var
    | DifferentTypes
    | DifferentRecordExtensions


data TypeClass
    = Comparable
    | Appendable
    | Number


isTypeClass :: String -> Maybe TypeClass
isTypeClass varName
    | any (/= '\'') primes = Nothing
    | name == "comparable" = Just Comparable
    | name == "appendable" = Just Appendable
    | name == "number"     = Just Number
    | otherwise            = Nothing
    where
      (name, primes) =
          break (=='\'') varName


type DocsComparison = Map.Map String [ComparisonEntry]


diffPackage :: (Value, Value) -> Parser DocsComparison
diffPackage (oldModules, newModules) =
  let moduleName :: Value -> Parser String
      moduleName v =
        do o <- getObject "module information" v
           o .: "name"

      addModule env v =
        do name <- moduleName v
           return $ Map.insert name v env

      processModule oldEnv compMap v =
        do name <- moduleName v
           case Map.lookup name oldEnv of
             Nothing -> return compMap
             Just v2 ->
               do comparison <- diffModule (v, v2)
                  return $ Map.insert name comparison compMap
  in
  case (oldModules, newModules) of
    (Array a1, Array a2) ->
      do env2 <- Vector.foldM addModule Map.empty a2
         Vector.foldM (processModule env2) Map.empty a1
    _ -> fail "Trying to parse documents from non-array"


diffModule :: (Value, Value) -> Parser [ComparisonEntry]
diffModule (oldModule, newModule) =
    case (oldModule, newModule) of
      (Object o1, Object o2) ->
        do (oldValues, newValues) <- extract "values"
           env1 <- buildEnv oldValues
           env2 <- buildEnv newValues
           entries <- mapMapM env1 $ \name (raw, value) ->
             do (state, raw2) <- buildEntry env2 name value
                return $ ComparisonEntry name raw raw2 state
           let f name (raw, _) ls =
                 case Map.lookup name env1 of
                   Nothing -> ComparisonEntry name raw Nothing Removed : ls
                   _ -> ls
           let entriesRemoved = Map.foldrWithKey f [] env2
           return $ entries ++ entriesRemoved
      _ -> fail "Tried to parse module information from non-object"
  where
    extractEntry :: Value -> Parser (Maybe (String, String, Value))
    extractEntry val =
      do o <- getObject "binding information" val
         exposed <- error "o .:? \"exposed\""
         name <- o .: "name"
         raw <- o .: "raw"
         typ <- o .: "type"
         return $
           case exposed of
             Just False -> Nothing
             _ -> Just (name, raw, typ)

    buildMap :: Ord a => [(a, b, c)] -> Map.Map a (b, c)
    buildMap = Map.fromList . map (\(x, y, z) -> (x, (y, z)))

    buildEnv :: [Value] -> Parser (Map.Map String (String, Value))
    buildEnv vs = buildMap . Maybe.catMaybes <$> mapM extractEntry vs

    mapMapM :: Monad m => Map.Map k v -> (k -> v -> m a) -> m [a]
    mapMapM m fn = Map.foldrWithKey g (return []) m
      where g k v act = liftM2 (:) (fn k v) act

    buildEntry :: Map.Map String (String, Value) -> String -> Value -> Parser (BindingState, Maybe String)
    buildEntry env name typ =
      case Map.lookup name env of
        Nothing -> return (Added, Nothing)
        Just (raw, typ2) ->
          do compat <- runErrorT $ diffType Map.empty (typ, typ2)
             return $ (\ x -> (x, Just raw)) $
               case compat of
                 Left _ -> Existing Incompatible
                 Right env -> Existing (compatibility env)


type Diff = ErrorT DiffError Parser

type NameUpdates = Map.Map String String

diffType :: NameUpdates -> (Value, Value) -> Diff NameUpdates
diffType nameDict (oldValue, newValue) =
    case (oldValue, newValue) of
      (Object old, Object new) ->
          do  (oldTag, newTag) <- getStrings old new "tag"
              assert (oldTag == newTag)
              case oldTag of
                "var" ->
                    diffVar nameDict old new
                "function" ->
                    diffFunction nameDict old new
                "adt" ->
                    diffAdt nameDict old new
                "alias" ->
                    error "is this actually correct? Diff.Compare.diffType - alias"
                "record" ->
                    diffRecord nameDict old new
                _ -> fail $ "Unknown tag " ++ oldTag

      _ -> fail "Trying to parse types from non-objects"


assert :: Bool -> Diff ()
assert condition =
    if condition then return () else throwError DifferentTypes


diffVar :: NameUpdates -> Object -> Object -> Diff NameUpdates
diffVar nameDict old new =
  do  (oldName, newName) <- getStrings old new "name"
      case Map.lookup oldName nameDict of
        Just revisedName ->
            if revisedName == newName
                then return nameDict
                else throwError $ DifferentRenaming oldName newName revisedName

        Nothing ->
            return (Map.insert oldName newName nameDict)


diffFunction :: NameUpdates -> Object -> Object -> Diff NameUpdates
diffFunction nameDict old new =
  do  (oldArgs, newArgs) <- getValueLists old new "args"
      assert (length oldArgs == length newArgs)
      nameDict' <- foldM diffType nameDict (zip oldArgs newArgs)

      (oldResult, newResult) <- getValues old new "result"
      diffType nameDict' (oldResult, newResult)


diffAdt :: NameUpdates -> Object -> Object -> Diff NameUpdates
diffAdt nameDict old new =
  do  (oldName, newName) <- getStrings old new "name"
      assert (oldName == newName)

      (oldArgs, newArgs) <- getValueLists old new "args"
      assert (length oldArgs == length newArgs)
      foldM diffType nameDict (zip oldArgs newArgs)


diffAlias :: NameUpdates -> Object -> Object -> Diff NameUpdates
diffAlias nameDict old new =
  do  (oldAlias, newAlias) <- getStrings old new "alias"
      assert (oldAlias == newAlias)
      return nameDict

diffRecord :: NameUpdates -> Object -> Object -> Diff NameUpdates
diffRecord nameDict old new =
  do  (oldExtension, newExtension) <- getOptionalValues old new "extensions"
      nameDict' <-
          case (oldExtension, newExtension) of
            (Nothing, Nothing) -> return nameDict
            (Just ov, Just nv) -> diffType nameDict (ov, nv)
            _ -> throwError DifferentTypes

      (oldFields, newFields) <- getKeyValuePairs old new "field"
      assert (length oldFields == length newFields)

      foldM (diffField (Map.fromList newFields)) nameDict' oldFields

  where
    diffField newFields nameDict (oldField, oldValue) =
        case Map.lookup oldField newFields of
          Just newValue ->
              diffType nameDict (oldValue, newValue)
          Nothing ->
              throwError DifferentTypes


-- JSON GETTERS

getObject :: String -> Value -> Parser Object
getObject name value =
    case value of
      Object o -> return o
      _ -> fail $ "Expected JSON object while parsing " ++ name


get :: FromJSON a => Object -> Object -> Text -> Parser (a, a)
get object1 object2 field =
  do  value1 <- (object1 .: field)
      value2 <- (object2 .: field)
      return (value1, value2)

getStrings :: Object -> Object -> Text -> RenameContext (String, String)
getStrings = get

getValues :: Object -> Object -> Text -> RenameContext (Value, Value)
getValues = get

getOptionalValues :: Object -> Object -> Text -> RenameContext (Maybe Value, Maybe Value)
getOptionalValues = get

getValueLists :: Object -> Object -> Text -> RenameContext ([Value], [Value])
getValueLists = get

getKeyValuePairs :: Object -> Object -> Text -> RenameContext ([(String, Value)], [(String, Value)])
getKeyValuePairs = get
