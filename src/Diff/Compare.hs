module Diff.Compare where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (zipWithM)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Diff.Model as M


-- CHANGE MAGNITUDE

data Magnitude
    = PATCH
    | MINOR
    | MAJOR
    deriving (Eq, Ord, Show)


packageChangeMagnitude :: PackageChanges -> Magnitude
packageChangeMagnitude pkgChanges =
    maximum (added : removed : map moduleChangeMagnitude moduleChanges)
  where
    moduleChanges =
        Map.elems (modulesChanged pkgChanges)

    removed =
        if null (modulesRemoved pkgChanges)
            then PATCH
            else MAJOR

    added =
        if null (modulesAdded pkgChanges)
            then PATCH
            else MINOR

moduleChangeMagnitude :: ModuleChanges -> Magnitude
moduleChangeMagnitude moduleChanges =
    maximum
        [ changeMagnitude (adtChanges moduleChanges)
        , changeMagnitude (aliasChanges moduleChanges)
        , changeMagnitude (valueChanges moduleChanges)
        ]

changeMagnitude :: Changes k v -> Magnitude
changeMagnitude (Changes added changed removed)
    | Map.size removed > 0 = MAJOR
    | Map.size changed > 0 = MAJOR
    | Map.size added   > 0 = MINOR
    | otherwise            = PATCH


-- DETECT CHANGES

data PackageChanges = PackageChanges
    { modulesAdded :: [String]
    , modulesChanged :: Map.Map String ModuleChanges
    , modulesRemoved :: [String]
    }

data ModuleChanges = ModuleChanges
    { adtChanges :: Changes String ([String], Map.Map String M.Type)
    , aliasChanges :: Changes String ([String], M.Type)
    , valueChanges :: Changes String M.Type
    }

data Changes k v = Changes
    { added :: Map.Map k v
    , changed :: Map.Map k (v,v)
    , removed :: Map.Map k v
    }


diffPackage :: M.Package -> M.Package -> PackageChanges
diffPackage (M.Package oldPackage) (M.Package newPackage) =
    PackageChanges
        (Map.keys added)
        (filterOutPatches (Map.map (uncurry diffModule) changed))
        (Map.keys removed)
  where
    filterOutPatches chngs =
        Map.filter (\chng -> moduleChangeMagnitude chng /= PATCH) chngs

    (Changes added changed removed) =
        getChanges (\_ _ -> False) oldPackage newPackage


diffModule :: M.Module -> M.Module -> ModuleChanges
diffModule (M.Module adts aliases values) (M.Module adts' aliases' values') =
    ModuleChanges
        (getChanges isEquivalentAdt adts adts')
        (getChanges isEquivalentType aliases aliases')
        (getChanges (\t t' -> isEquivalentType ([],t) ([],t')) values values')


getChanges :: (Ord k) => (v -> v -> Bool) -> Map.Map k v -> Map.Map k v -> Changes k v
getChanges isEquivalent old new =
    Changes
    { added =
        Map.difference new old
    , changed =
        Map.filter
            (not . uncurry isEquivalent)
            (Map.intersectionWith (,) old new)
    , removed =
        Map.difference old new
    }


isEquivalentAdt :: ([String], Map.Map String M.Type) -> ([String], Map.Map String M.Type) -> Bool
isEquivalentAdt (oldVars, oldCtors) (newVars, newCtors) =
    Map.size oldCtors == Map.size newCtors
    && and (zipWith (==) (Map.keys oldCtors) (Map.keys newCtors))
    && and (Map.elems (Map.intersectionWith equiv oldCtors newCtors))
  where
    equiv oldType newType =
        isEquivalentType (oldVars, oldType) (newVars, newType)


isEquivalentType :: ([String], M.Type) -> ([String], M.Type) -> Bool
isEquivalentType (oldVars, oldType) (newVars, newType) =
    case diffType oldType newType of
      Nothing -> False
      Just renamings ->
          length oldVars == length newVars
          && isEquivalentRenaming (zip oldVars newVars ++ renamings)


-- TYPES

diffType :: M.Type -> M.Type -> Maybe [(String,String)]
diffType oldType newType =
    case (oldType, newType) of
      (M.Var oldName, M.Var newName) ->
          Just [(oldName, newName)]

      (M.Type oldName, M.Type newName) ->
          if oldName == newName
              then Just []
              else Nothing

      (M.Lambda a b, M.Lambda a' b') ->
          (++)
              <$> diffType a a'
              <*> diffType b b'

      (M.App a b, M.App a' b') ->
          (++)
              <$> diffType a a'
              <*> diffType b b'

      (M.Record fields maybeExt, M.Record fields' maybeExt') ->
          case (maybeExt, maybeExt') of
            (Nothing, Just _) -> Nothing
            (Just _, Nothing) -> Nothing
            (Nothing, Nothing) -> diffFields fields fields'
            (Just ext, Just ext') ->
                (++)
                    <$> diffType ext ext'
                    <*> diffFields fields fields'

      (_, _) ->
          Nothing


diffFields :: [(String, M.Type)] -> [(String, M.Type)] -> Maybe [(String,String)]
diffFields rawFields rawFields'
    | length rawFields /= length rawFields' = Nothing
    | or (zipWith ((/=) `on` fst) fields fields') = Nothing
    | otherwise =
        concat <$> zipWithM (diffType `on` snd) fields fields'
    where
      fields  = sort rawFields
      fields' = sort rawFields'

      sort =
          List.sortBy (compare `on` fst)


-- TYPE VARIABLES

isEquivalentRenaming :: [(String,String)] -> Bool
isEquivalentRenaming varPairs =
    case mapM verify renamings of
      Nothing -> False
      Just verifiedRenamings ->
          allUnique (map snd verifiedRenamings)
  where
    renamings =
        Map.toList (foldr insert Map.empty varPairs)

    insert (old,new) dict =
        Map.insertWith (++) old [new] dict

    verify (old, news) =
        case news of
          [] -> Nothing
          new : rest ->
              if all (new ==) rest
                  then Just (old, new)
                  else Nothing

    allUnique list =
        length list == Set.size (Set.fromList list)


compatableVars :: String -> String -> Bool
compatableVars old new =
    case (categorizeVar old, categorizeVar new) of
      (Comparable, Comparable) -> True
      (Appendable, Appendable) -> True
      (Number    , Number    ) -> True

      (Comparable, Appendable) -> True
      (Number    , Comparable) -> True

      (_, Var) -> True

      (_, _) -> False


data TypeVarCategory
    = Comparable
    | Appendable
    | Number
    | Var


categorizeVar :: String -> TypeVarCategory
categorizeVar varName
    | any (/= '\'') primes = Var
    | name == "comparable" = Comparable
    | name == "appendable" = Appendable
    | name == "number"     = Number
    | otherwise            = Var
    where
      (name, primes) =
          break (=='\'') varName
