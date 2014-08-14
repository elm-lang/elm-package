{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.SemverCheck where

import Control.Monad.Error
import Data.Aeson hiding (Number)
import Data.Aeson.Types (Parser)
import Data.Functor ((<$>))
import Data.Text (Text)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V

-- DATATYPE DEFINITIONS

{-| A type for all possible reasons why one version isn't an
immediate successor to another version -}
data VersionError
  = NotSuccessor Int Int
  | NotZero Int
  | SameVersions
  | TooLongVersion
  deriving (Show)

{-| Datatype of all possible parts of version number
according to semantic versioning -}
data IndexPos
  = Major
  | Minor
  | Patch
  deriving (Show)

-- | A compatibility relation between two "things", e.g. function types
data Compatibility
  = Incompatible
  | Compatible
  | Same
  deriving (Eq, Ord, Show)

-- | Datatype of all possible reasons why particular types are incompatible
data NonCorrespondence
  = DifferentRenaming Var Var Var
  | DifferentTypes
  | DifferentRecordExtensions
  | ParsingError
  | StringError String

{-| There are special type variables in Elm, like "comparable''", which could
be instantiated only to a particular concrete types. This datatype enumerates
all type variable sorts -}
data VariableType
  = Comparable
  | Appendable
  | Number
  | Regular
  deriving (Eq, Show)

{-| An information about how particular definition in module changed
with respect to the previous version of the same module -}
data BindingState
  = Added
  | Existing Compatibility
  | Removed
  deriving (Eq, Show, Ord)

{-| All information about particular bniding in a module -}
data ComparisonEntry = ComparisonEntry
    { name :: String
    , raw :: String
    , raw2 :: Maybe String
    , state :: BindingState
    } deriving (Show)

showIndexPos :: IndexPos -> String
showIndexPos x = map Char.toLower (show x) ++ " position"

buildIndexPos :: Int -> Either (Int, VersionError) IndexPos
buildIndexPos x =
  case x of
    0 -> Right Major
    1 -> Right Minor
    2 -> Right Patch
    _ -> Left (x, TooLongVersion)

showIntAsIndex :: Int -> String
showIntAsIndex x =
  case buildIndexPos x of
    Left _ -> "position " ++ show (x + 1)
    Right v -> showIndexPos v

{-| Check two version numbers to see whether one of them follows another.
If that's the case, return an index in version number when increment
takes place. If that's not the case, return an error with position
where that error happened -}
immediateNext :: [Int] -> [Int] -> Either (Int, VersionError) IndexPos
immediateNext prev next = check 0 $ zip (prev ++ repeat 0) next
  where check i ls =
          case ls of
            [] -> Left (i, SameVersions)
            (x, y) : rest
              | x == y -> check (i + 1) rest
              | x + 1 == y -> checkZeros i (i + 1) rest
              | otherwise -> Left (i, NotSuccessor x y)

        checkZeros result pos ls =
          case ls of
            [] -> buildIndexPos result
            (_, 0) : rest -> checkZeros result (pos + 1) rest
            (_, y) : _ -> Left (pos, NotZero y)

-- | A class of things which could be compared for compatibility
class Change a where
  compatibility :: a -> Compatibility

instance Change a => Change [a] where
  compatibility ls = minimum (Same : map compatibility ls)

instance Change (VariableType, VariableType) where
  compatibility (ty1, ty2) =
    case (ty1, ty2) of
      (Regular, Regular) -> Same
      (_, Regular) -> Compatible
      (x, y) | x == y -> Same
             | otherwise -> Incompatible

instance Change VarRenaming where
  compatibility = Map.foldrWithKey f Same
    where f v1 v2 k = compatibility (variableType v1, variableType v2) `min` k

instance Change BindingState where
  compatibility st = case st of
    Removed -> Incompatible
    Existing comp -> comp
    Added -> Compatible

instance Change ComparisonEntry where
  compatibility = compatibility . state

type Var = String

-- | Error instance for NonCorrespondence specially for using ErrorT transformer
instance Error NonCorrespondence where
  noMsg = StringError "Unknown error"
  strMsg = StringError

{-| Check whether second argument consists of first argument and
zero or more ticks -}
isSpecial :: String -> String -> Bool
isSpecial prefix str =
  let n = length prefix
  in (List.isPrefixOf prefix str) && (all (== '\'') $ drop n str)

-- | Get a type variable sort by its name
variableType :: String -> VariableType
variableType str
  | isSpecial "comparable" str = Comparable
  | isSpecial "appendable" str = Appendable
  | isSpecial "number" str = Number
  | otherwise = Regular

type VarRenaming = Map.Map Var Var

type RenameContext = ErrorT NonCorrespondence Parser

-- | Pretty-printing function for module comparison entries
showEntry :: ComparisonEntry -> [String]
showEntry (ComparisonEntry _ r mr2 s) =
  case (s, mr2) of
    (Existing _, Just r2) ->
      [ "  - " ++ r2
      , "  + " ++ r
      , ""]
    _ -> ["    " ++ r]

{-| Add an (indented) top line for non-empty list of strings.
Leave empty list of strings as it is -}
addPrefix :: Int -> String -> [String] -> [String]
addPrefix len pref ls =
  case ls of
    [] -> []
    _ -> ((take len (repeat ' ') ++ pref) : ls)

-- | Pretty-print module comparison given as a list of entries
renderEntries :: [ComparisonEntry] -> [String]
renderEntries entries =
  concat [ addPrefix 2 "Added:" $ getDocs Added
         , addPrefix 2 "Changed:" $
           getManyDocs [Existing Compatible, Existing Incompatible]
         , addPrefix 2 "Removed:" $ getDocs Removed ]
  where
    sortedEntries = foldr insertItem Map.empty entries

    getDocs key = Maybe.fromMaybe [] $ Map.lookup key sortedEntries
    getManyDocs keys = concatMap getDocs keys

    insertItem entry m =
      case state entry of
        Existing Same -> m
        st -> Map.insertWith (++) st (showEntry entry) m

-- | Pretty-print comparison of whole docs.json
renderDocsComparison :: Map.Map String [ComparisonEntry] -> [String]
renderDocsComparison = Map.foldrWithKey attach []
  where
    attach name entries ls =
      addPrefix 0 ("Module " ++ name) (renderEntries entries) ++ ls

generalExtract :: FromJSON a => Object -> Object -> Text -> Parser (a, a)
generalExtract o1 o2 tag =
  do v1 <- (o1 .: tag)
     v2 <- (o2 .: tag)
     return (v1, v2)

expectObject :: String -> Value -> Parser Object
expectObject name val =
  case val of
    Object o -> return o
    _ -> fail $ "Expected JSON object while parsing " ++ name

-- | Build a map of differences between two "docs.json"
buildDocsComparison :: (Value, Value) -> Parser (Map.Map String [ComparisonEntry])
buildDocsComparison (v1, v2) =
  let moduleName :: Value -> Parser String
      moduleName v =
        do o <- expectObject "module information" v
           o .: "name"

      addModule env v =
        do name <- moduleName v
           return $ Map.insert name v env

      processModule oldEnv compMap v =
        do name <- moduleName v
           case Map.lookup name oldEnv of
             Nothing -> return compMap
             Just v2 ->
               do comparison <- buildModuleComparison (v, v2)
                  return $ Map.insert name comparison compMap
  in
  case (v1, v2) of
    (Array a1, Array a2) ->
      do env2 <- V.foldM addModule Map.empty a2
         V.foldM (processModule env2) Map.empty a1
    _ -> fail "Trying to parse documents from non-array"

-- | Build a list of differences between two versions of a module
buildModuleComparison :: (Value, Value) -> Parser [ComparisonEntry]
buildModuleComparison (v1, v2) =
  case (v1, v2) of
    (Object o1, Object o2) ->
      let extract :: FromJSON a => Text -> Parser (a, a)
          extract = generalExtract o1 o2

          extractEntry :: Value -> Parser (Maybe (String, String, Value))
          extractEntry val =
            do o <- expectObject "binding information" val
               exposed <- o .:? "exposed"
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
                do compat <- runErrorT $ buildRenaming Map.empty (typ, typ2)
                   return $ (,Just raw) $
                     case compat of
                       Left _ -> Existing Incompatible
                       Right env -> Existing (compatibility env)

      in
      do (vs1, vs2) <- extract "values"
         env1 <- buildEnv vs1
         env2 <- buildEnv vs2
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

{-| Function to build a renaming of variables application of which
transforms first type to another. Type signature represented as JSON values,
as serialized by Elm.Internal.Documentation in "Elm" library.
First value is of newer module, second is of older. -}
buildRenaming :: VarRenaming -> (Value, Value) -> RenameContext VarRenaming
buildRenaming env (v1, v2) =
  case (v1, v2) of
    (Object o1, Object o2) ->
      let extract :: FromJSON a => Text -> RenameContext (a, a)
          extract = lift . generalExtract o1 o2

          assert cond = when (not cond) $ throwError DifferentTypes
      in
      do (tag1 :: String, tag2) <- extract "tag"
         assert (tag1 == tag2)
         case tag1 of
           "var" ->
             do (var1, var2) <- extract "name"
                case Map.lookup var1 env of
                  Just rvar ->
                    case (rvar == var2) of
                      True -> return env
                      False -> throwError $ DifferentRenaming var1 var2 rvar
                  Nothing ->
                    return (Map.insert var1 var2 env)

           "function" ->
             do (ts1, ts2) <- extract "args"
                (r1, r2) <- extract "result"

                assert (length ts1 == length ts2)
                env1 <- foldM buildRenaming env (zip ts1 ts2)
                buildRenaming env1 (r1, r2)

           "adt" ->
             do (n1 :: String, n2) <- extract "name"
                assert (n1 == n2)

                (a1, a2) <- extract "args"
                assert (length a1 == length a2)
                foldM buildRenaming env (zip a1 a2)

           "alias" ->
             do (n1 :: String, n2) <- extract "alias"
                assert (n1 == n2)
                return env

           "record" ->
             do (ext1, ext2) <- extract "extensions"
                env1 <-
                  case (ext1, ext2) of
                    (Nothing, Nothing) -> return env
                    (Just v1, Just v2) -> buildRenaming env (v1, v2)
                    _ -> throwError DifferentTypes

                (fs1 :: [(String, Value)], fs2) <- extract "field"
                assert (length fs1 == length fs2)
                let map2 = Map.fromList fs2
                    f e (name, val) =
                      case Map.lookup name map2 of
                        Just val2 -> buildRenaming e (val, val2)
                        Nothing -> throwError DifferentTypes
                foldM f env1 fs1

           _ -> fail $ "Unknown tag " ++ tag1

    _ -> fail "Trying to parse types from non-objects"
