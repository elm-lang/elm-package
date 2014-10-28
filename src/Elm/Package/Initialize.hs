{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Initialize (descriptionAndSolution, solution) where

import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)

import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as S
import qualified Install
import qualified Manager


descriptionAndSolution :: (MonadError String m, MonadIO m) => m S.Solution
descriptionAndSolution =
    runInstall (Install.Latest (N.Name "elm-lang" "core"))


solution :: (MonadError String m, MonadIO m) => m S.Solution
solution =
    runInstall Install.Everything


runInstall :: (MonadError String m, MonadIO m) => Install.Args -> m S.Solution
runInstall args =
  do  either <- liftIO $ do
          env <- Manager.defaultEnvironment
          Manager.run env (Install.install args)

      case either of
        Left err -> throwError err
        Right () ->
            S.read Path.solvedDependencies
