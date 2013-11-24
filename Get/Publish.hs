{-# LANGUAGE OverloadedStrings #-}
module Get.Publish where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Error
import System.Directory
import System.FilePath (replaceExtension, (</>))
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.ByteString as BS

import qualified Get.Utils          as Utils
import qualified Get.Registry       as R
import qualified Model.Dependencies as D
import qualified Model.Name         as N
import qualified Model.Version      as V

publish :: ErrorT String IO ()
publish =
  do deps <- D.depsAt Utils.depsFile
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Utils.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyExposedModules exposedModules
     verifyVersion name version
     generateDocs exposedModules
     liftIO $ R.send $ R.register name version Utils.combinedJson
     Utils.out "Success!"

verifyExposedModules :: [String] -> ErrorT String IO ()
verifyExposedModules modules =
    do when (null modules) $ throwError $
              "There are no exposed modules! All libraries must make at \
              \least one module available to users."
       mapM_ verifyExists modules
    where
      verifyExists modul =
          let path = Utils.moduleToElmFile modul in
          do exists <- liftIO $ doesFileExist path
             when (not exists) $ throwError $
                 "Cannod find module " ++ modul ++ " at " ++ path

verifyVersion :: N.Name -> V.Version -> ErrorT String IO ()
verifyVersion name version =
    do maybeDeps <- liftIO $ R.send (R.metadata name)
       case maybeDeps of
         Nothing -> return ()
         Just oldDeps ->
             let oldVersion = D.version oldDeps in
             when (oldVersion >= version) $ throwError $ unlines
                 [ "a later version has already been released."
                 , "Use a version number higher than " ++ show oldVersion ]

generateDocs :: [String] -> ErrorT String IO ()
generateDocs modules = 
    do forM elms $ \path -> Utils.run "elm-doc" [path]
       liftIO $ do
         let path = Utils.combinedJson
         BS.writeFile path "[\n"
         let addCommas = List.intersperse (BS.appendFile path ",\n")
         sequence_ $ addCommas $ map append jsons
         BS.appendFile path "\n]"

    where
      elms = map Utils.moduleToElmFile modules
      jsons = map Utils.moduleToJsonFile modules

      append :: FilePath -> IO ()
      append path = do
        json <- BS.readFile path
        BS.length json `seq` return ()
        BS.appendFile Utils.combinedJson json
