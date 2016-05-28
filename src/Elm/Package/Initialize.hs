{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Initialize (solution) where

import Control.Monad.Except (ExceptT, MonadIO, liftIO, throwError)
import qualified Data.Map as Map
import System.Directory (doesFileExist)

import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as S
import qualified Install
import qualified Manager
import qualified Reporting.Error as Error



solution :: (MonadIO m) => Bool -> ExceptT String m S.Solution
solution autoYes =
  do  result <- liftIO $ Manager.run $ installEverythingAndGetSolution autoYes
      case result of
        Right solution ->
          return solution

        Left err ->
          throwError $ Error.toString err


installEverythingAndGetSolution :: Bool -> Manager.Manager S.Solution
installEverythingAndGetSolution autoYes =
  do  () <- Install.install autoYes Install.Everything
      exists <- liftIO (doesFileExist Path.solvedDependencies)
      if exists
        then S.read Error.CorruptSolution Path.solvedDependencies
        else return Map.empty
