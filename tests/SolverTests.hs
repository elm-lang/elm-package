module SolverTests (solverTests) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Test.HUnit ((@?), Assertion)
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TH
import qualified Utils.ResolveDeps as Deps
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Elm.Internal.Constraint as C
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Name as N


-- SETUP

type FakeDB = Map N.Name [(V.Version, Deps.Constraints)]

db1 :: FakeDB
db1 = Map.fromList [ base, transformers, mtl, conduit, http ]
  where
    base =
      (n "base", [ (v "0.1", []), (v "0.2", []), (v "1.0", []), (v "1.1", []) ])

    transformers =
      (n "transformers", [ (v "1.0", [(n "base", c ">=1.0 <2.0")]) ])

    mtl =
      (n "mtl", [ (v "1.0", [ (n "base", c ">=0.2 <1.0") ])
                , (v "2.0", [ (n "base", c ">=0.2 <2.0")
                            , (n "transformers", c ">=1.0 <2.0")
                            ])
                ])

    conduit =
      (n "conduit", [ (v "1.0", [ (n "base", c ">=1.0 <1.1")
                                , (n "http", c ">=2.0 <3.0")
                                ])
                    ])

    http =
      (n "http", [ (v "2.1", [ (n "base", c ">=1.1 <2.0") ]) ])

expectSolution :: FakeDB -> N.Name -> V.Version -> Assertion
expectSolution db name version =
  do solution <- fromError $ solveFake db name version
     isValidSolution db solution @? "Solution should pass sanity check"

expectNoSolution :: FakeDB -> N.Name -> V.Version -> Assertion
expectNoSolution db name version =
  let solution = runIdentity $ runErrorT $ solveFake db name version
  in case solution of
       Left err -> return ()
       Right sol ->
         do when (isValidSolution db sol) $ putStrLn "FAULTY TESTS: solution is valid!"
            False @? ("Unexpected solution " ++ show sol)

test1 = expectSolution db1 (n "mtl") (v "1.0")
test2 = expectSolution db1 (n "mtl") (v "2.0")
test3 = expectNoSolution db1 (n "conduit") (v "1.0")

solverTests =
  TF.testGroup "Dependency solver tests"
  [ TH.testCase "test1" test1
  , TH.testCase "test2" test2
  , TH.testCase "test3" test3
  ]


fromError :: ErrorT String Identity a -> IO a
fromError action =
  either fail return $ runIdentity (runErrorT action)

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
        versions <- Map.lookup name db
        constraints <- lookup version versions
        return (all isSatisfied constraints)

    isSatisfied (name, constraint) =
      maybe False id $ do
        version <- lookup name solution
        return (C.satisfyConstraint constraint version)


-- RUN SOLVER

-- | Run dependency solver using stub data
solveFake :: FakeDB -> N.Name -> V.Version -> ErrorT String Identity [(N.Name, V.Version)]
solveFake db name version =
  do let libraryDb = toLibraryDb db
         unreader = runReaderT (Deps.solveForVersion name version) $
                    Deps.SolverEnv libraryDb (readConstraints db)
         initialState = Deps.SolverState Map.empty
     (solved, _) <- runStateT unreader initialState
     return $ Map.toList solved

-- | A function passed to solver which "reads" constraints by name and version
readConstraints :: FakeDB -> N.Name -> V.Version -> ErrorT String Identity Deps.Constraints
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
toLibraryDb :: FakeDB -> Deps.LibraryDB
toLibraryDb fakeDb =
    Map.fromList (map toLibraryEntry (Map.toList fakeDb))
  where
    toLibraryEntry (name, details) =
      ( N.toString name
      , Deps.LibraryInfo (N.toString name) "" (map fst details)
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


