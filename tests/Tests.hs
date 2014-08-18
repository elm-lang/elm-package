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

-- | Differs from Data.Maybe.fromJust by having a mandatory error message
unsafeUnpackJust :: String -> Maybe a -> a
unsafeUnpackJust msg result =
  case result of
    Just v -> v
    Nothing -> error $ "error unpacking " ++ msg ++ " from string"

type FakeDB = Map N.Name [(V.Version, Constraints)]

-- | Helper function to write inline test data more succintly
v :: String -> V.Version
v = unsafeUnpackJust "version" . V.fromString

c :: String -> C.Constraint
c = unsafeUnpackJust "constraint" . C.fromString

n :: String -> N.Name
n = unsafeUnpackJust "name" . N.fromString . ("a/" ++)

db1 :: FakeDB
db1 = Map.fromList
  [ (n "base", [(v "0.1", []), (v "0.2", []), (v "1.0", [])])
  , (n "transformers", [(v "1.0", [(n "base", c ">=1.0 <2.0")])])
  , (n "mtl", [ (v "1.0", [(n "base", c ">=0.2 <1.0")])
              , (v "2.0", [ (n "base", c ">=0.2 <2.0")
                          , (n "transformers", c ">=1.0 <2.0")])])
  ]

{-| Extract from stub data list of libraries and their version in
format solver expects
-}
fakeLibraryDb :: FakeDB -> LibraryDB
fakeLibraryDb = Map.fromList . map (uncurry f) . Map.toList
  where
    f name ls = (N.toString name, LibraryInfo (N.toString name) "" (map fst ls))

-- | A function passed to solver which "reads" constraints by name and version
readConstraints :: FakeDB -> N.Name -> V.Version -> ErrorT String IO Constraints
readConstraints db name version =
  let result =
        do versions <- Map.lookup name db
           lookup version versions
  in
  case result of
    Just r -> return r
    Nothing -> throwError $ "Haven't found " ++ N.toString name ++ " " ++ V.toString version

-- | Run dependency solver using stub data
solveFake :: FakeDB -> N.Name -> V.Version -> ErrorT String IO [(N.Name, V.Version)]
solveFake db name version =
  do constraints <- readConstraints db name version
     let libraryDb = fakeLibraryDb db
         unreader = runReaderT (solveConstraintsByDeps name version constraints) $
                    SolverEnv libraryDb (readConstraints db)
         initialState = SolverState Map.empty Map.empty
     (solved, state) <- runStateT unreader initialState
     case solved of
       False -> throwError "Failed to satisfy all the constraints :-("
       True -> return $ Map.toList $ ssPinnedVersions state

{-| Check whether given solution is really a solution. Supposed to use as
a sanity check for existing tests and solver solutions
-}
satisfyConstraints :: FakeDB -> [(N.Name, V.Version)] -> Bool
satisfyConstraints db solution = all f solution
  where f (name, version) =
          case Map.lookup name db of
            Nothing -> False
            Just ls ->
              case lookup version ls of
                Nothing -> False
                Just constraints ->
                  all g constraints
        g (name, constraint) =
          case lookup name solution of
            Just version -> C.satisfyConstraint constraint version
            Nothing -> False

-- | Unpack ErrorT in IO: throw transformer's error as user IO error
fromError :: ErrorT String IO a -> IO a
fromError action =
  do result <- runErrorT action
     case result of
       Right value -> return value
       Left err -> fail err

test1 = fromError $ solveFake db1 (n "mtl") (v "1.0")
test2 = fromError $ solveFake db1 (n "mtl") (v "2.0")
test2sol = [(n "base", v "1.0"), (n "transformers", v "1.0"), (n "mtl", v "2.0")]

main :: IO ()
main =
  do test1 >>= print
     test2 >>= print
