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
     R.send $ R.register name version Utils.combinedJson
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
    do response <- R.send (R.versions name)
       case response of
         Nothing -> return ()
         Just versions ->
             do let maxVersion = maximum (version:versions)
                when (version < maxVersion) $ throwError $ unlines
                     [ "a later version has already been released."
                     , "Use a version number higher than " ++ show maxVersion ]
                checkSemanticVersioning maxVersion

       checkTag version

    where
      checkSemanticVersioning _ = return ()

      checkTag version = do
        tags <- lines <$> Utils.git [ "tag", "--list" ]
        let v = show version
        when (show version `notElem` tags) $ do
          yes <- liftIO $ do
                   mapM putStrLn (tagMessage v)
                   putStr "Would you like me to run these commands for you? (y/n): "
                   Utils.yesOrNo
          case yes of
            False -> throwError "you must publicly tag your repo before publishing."
            True -> do Utils.git ["tag", "-a " ++ v, "-m 'release version " ++ v ++ "'"]
                       Utils.git ["push", "origin", v]
                       return ()

      tagMessage v =
          [ "Libraries must be tagged before they are published. This makes it possible to"
          , "find this specific version on github. You can run the following commands to tag"
          , "the current code and push it to github:"
          , ""
          , "    git tag --annotate " ++ v ++ " --message 'release version " ++ v ++ "'"
          , "    git push origin " ++ v
          , "" ]

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
