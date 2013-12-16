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

import Data.Version
import qualified Paths_elm_get             as This
import qualified Get.Registry              as R
import qualified Utils.Paths               as Path
import qualified Utils.Commands            as Cmd
import qualified Utils.Http                as Http
import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Paths        as EPath
import qualified Elm.Internal.Name         as N
import qualified Elm.Internal.Version      as V

publish :: ErrorT String IO ()
publish =
  do deps <- D.depsAt EPath.dependencyFile
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyElmVersion (D.elmVersion deps)
     verifyExposedModules exposedModules
     verifyVersion name version
     withCleanup $ do
       generateDocs exposedModules
       R.register name version Path.combinedJson
     Cmd.out "Success!"

withCleanup :: ErrorT String IO () -> ErrorT String IO ()
withCleanup action =
    do existed <- liftIO $ doesDirectoryExist "docs"
       either <- liftIO $ runErrorT action
       when (not existed) $ liftIO $ removeDirectoryRecursive "docs"
       case either of
         Left err -> throwError err
         Right () -> return ()

verifyElmVersion :: V.Version -> ErrorT String IO ()
verifyElmVersion elmVersion@(V.V ns _)
    | ns == ns' = return ()
    | otherwise =
        throwError $ "elm_dependencies.json says this project depends on version " ++
                     show elmVersion ++ " of the compiler but the compiler you " ++
                     "have installed is version " ++ showVersion This.version
    where
      Version ns' _ = This.version

verifyExposedModules :: [String] -> ErrorT String IO ()
verifyExposedModules modules =
    do when (null modules) $ throwError $
              "There are no exposed modules! All libraries must make at \
              \least one module available to users."
       mapM_ verifyExists modules
    where
      verifyExists modul =
          let path = Path.moduleToElmFile modul in
          do exists <- liftIO $ doesFileExist path
             when (not exists) $ throwError $
                 "Cannot find module " ++ modul ++ " at " ++ path

verifyVersion :: N.Name -> V.Version -> ErrorT String IO ()
verifyVersion name version =
    do response <- R.versions name
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
        tags <- lines <$> Cmd.git [ "tag", "--list" ]
        let v = show version
        when (show version `notElem` tags) $
             throwError (unlines (tagMessage v))

      tagMessage v =
          [ "Libraries must be tagged in git, but tag " ++ v ++ " was not found."
          , "These tags make it possible to find this specific version on github."
          , "To tag the most recent commit and push it to github, run this:"
          , ""
          , "    git tag -a " ++ v ++ " -m \"release version " ++ v ++ "\""
          , "    git push origin " ++ v
          , ""
          ]

generateDocs :: [String] -> ErrorT String IO ()
generateDocs modules = 
    do forM elms $ \path -> Cmd.run "elm-doc" [path]
       liftIO $ do
         let path = Path.combinedJson
         BS.writeFile path "[\n"
         let addCommas = List.intersperse (BS.appendFile path ",\n")
         sequence_ $ addCommas $ map append jsons
         BS.appendFile path "\n]"

    where
      elms = map Path.moduleToElmFile modules
      jsons = map Path.moduleToJsonFile modules

      append :: FilePath -> IO ()
      append path = do
        json <- BS.readFile path
        BS.length json `seq` return ()
        BS.appendFile Path.combinedJson json
