{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Solver (solve) where

import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)

import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Name as N
import qualified Elm.Package.Solution as S
import qualified Install.Solver as Solver
import qualified Manager


solve
    :: (MonadError String m, MonadIO m)
    => [(N.Name, C.Constraint)] -> m S.Solution
solve constraints =
  do  either <- liftIO $ do
          env <- Manager.defaultEnvironment
          Manager.run env (Solver.solve constraints)

      case either of
        Left err -> throwError err
        Right solution -> return solution
