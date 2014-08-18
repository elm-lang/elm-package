module Main where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Utils.ResolveDeps
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Elm.Internal.Constraint as C
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Name as N


-- SETUP

type FakeDB = Map N.Name [(V.Version, Constraints)]

db1 :: FakeDB
db1 = Map.fromList [ base, transformers, mtl ]
  where
    base =
      (n "base", [ (v "0.1", []), (v "0.2", []), (v "1.0", []) ])

    transformers =
      (n "transformers", [ (v "1.0", [(n "base", c ">=1.0 <2.0")]) ])
    
    mtl =
      (n "mtl", [ (v "1.0", [ (n "base", c ">=0.2 <1.0") ])
                , (v "2.0", [ (n "base", c ">=0.2 <2.0")
                            , (n "transformers", c ">=1.0 <2.0")
                            ])
                ])

test1 = fromError $ solveFake db1 (n "mtl") (v "1.0")
test2 = fromError $ solveFake db1 (n "mtl") (v "2.0")
test2sol = [(n "base", v "1.0"), (n "transformers", v "1.0"), (n "mtl", v "2.0")]

main :: IO ()
main =
  do test1 >>= print
     test2 >>= print

fromError :: ErrorT String IO a -> IO a
fromError action =
  either fail return =<< runErrorT action


-- CHECK FOR VALIDITY OF SOLUTIONS

{-| Check whether given solution is really a solution. Supposed to use as
a sanity check for existing tests and solver solutions
-}
isValidSolution :: FakeDB -> [(N.Name, V.Version)] -> Bool
isValidSolution db solution =
    all isConsistent solution
  where
    isConsistent (name, version) =
      maybe False id $ do
        versions <- Map.lookup name db of
        constraints <- lookup version versions
        return (all isSatisfied constraints)

    isSatisfied (name, constraint) =
      maybe False id $ do
        version <- lookup name solution
        return (C.satisfyConstraint constraint version)


-- RUN SOLVER

-- | Run dependency solver using stub data
solveFake :: FakeDB -> N.Name -> V.Version -> ErrorT String IO [(N.Name, V.Version)]
solveFake db name version =
  do constraints <- readConstraints db name version
     let libraryDb = toLibraryDb db
         unreader = runReaderT (solveConstraintsByDeps name version constraints) $
                    SolverEnv libraryDb (readConstraints db)
         initialState = SolverState Map.empty Map.empty
     (solved, state) <- runStateT unreader initialState
     case solved of
       False -> throwError "Failed to satisfy all the constraints :-("
       True -> return $ Map.toList $ ssPinnedVersions state

-- | A function passed to solver which "reads" constraints by name and version
readConstraints :: FakeDB -> N.Name -> V.Version -> ErrorT String IO Constraints
readConstraints db name version =
    maybe notFound return $ do
      versions <- Map.lookup name db
      lookup version versions
  where
    notFound =
      throwError $ "Could not find " ++ N.toString name ++ " " ++ V.toString version

{-| Extract from stub data list of libraries and their version in
format solver expects
-}
toLibraryDb :: FakeDB -> LibraryDB
toLibraryDb fakeDb =
    Map.fromList (map toLibraryEntry (Map.toList fakeDb))
  where
    toLibraryEntry (name, details) =
      ( N.toString name
      , LibraryInfo (N.toString name) "" (map fst details)
      )


-- SETUP HELPERS
-- make it easier to create a FakeDb

v :: String -> V.Version
v = unsafeUnpackJust "version" . V.fromString

c :: String -> C.Constraint
c = unsafeUnpackJust "constraint" . C.fromString

n :: String -> N.Name
n = unsafeUnpackJust "name" . N.fromString . ("a/" ++)

unsafeUnpackJust :: String -> Maybe a -> a
unsafeUnpackJust msg result =
  case result of
    Just v -> v
    Nothing -> error $ "error unpacking " ++ msg ++ " from string"


