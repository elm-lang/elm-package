module Get.Publish where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Error
import qualified Data.Maybe as Maybe

import qualified Get.Utils as Utils
import qualified Get.Registry as Registry
import qualified Model.Dependencies as D
import qualified Model.Version as V

publish :: ErrorT String IO ()
publish =
  do deps <- D.depsAt Utils.depsFile
     let library = D.name deps
         version = D.version deps
     Utils.out $ unwords [ "Verifying that", show library, show version, "is ready for release." ]
     latest <- Registry.metadata library
     case latest of
       Just oldDeps ->
           let oldVersion = D.version oldDeps in
           when (oldVersion >= version) $ throwError $ unlines
                    [ "a later version (" ++ show oldVersion ++ ") has already been released."
                    , "Use a higher version number." ]
       _ -> return ()
     Utils.out "now we publish"
     return ()
