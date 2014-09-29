{-# LANGUAGE OverloadedStrings #-}
module Publish where

import Control.Applicative ((<$>))
import Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Directory
import System.Exit
import System.IO

import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Description as Package
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as P
import qualified Elm.Package.Version as V

import qualified Get.Registry as R
import qualified Utils.Commands as Cmd
import qualified Utils.Paths as Path

publish :: ErrorT String IO ()
publish =
  do desc <- getDescription
     let name = Package.name desc
         version = Package.version desc
         exposedModules = Package.exposed desc
     Cmd.out $ unwords [ "Verifying", N.toString name, V.toString version, "..." ]
     verifyNoDependencies (Package.dependencies desc)
     verifyElmVersion (Package.elmVersion desc)
     verifyMetadata desc
     verifyExposedModulesExist exposedModules
     verifyVersion name version
     withCleanup $ do
       generateDocs exposedModules
       R.register name version Path.combinedJson
     Cmd.out "Success!"

getDescription :: ErrorT String IO Package.Description
getDescription =
  do either <- liftIO $ runErrorT Package.read
     case either of
       Right desc -> return desc
       Left err ->
           liftIO $ do
             hPutStrLn stderr $ "\nError: " ++ err
             exitFailure

withCleanup :: ErrorT String IO () -> ErrorT String IO ()
withCleanup action =
    do existed <- liftIO $ doesDirectoryExist "docs"
       either <- liftIO $ runErrorT action
       when (not existed) $ liftIO $ removeDirectoryRecursive "docs"
       case either of
         Left err -> throwError err
         Right () -> return ()

verifyNoDependencies :: [(N.Name,C.Constraint)] -> ErrorT String IO ()
verifyNoDependencies [] = return ()
verifyNoDependencies _ =
    throwError
        "elm-get is not able to publish projects with dependencies yet. This is a\n\
        \very high proirity, we are working on it! For now, announce your library on the\n\
        \mailing list: <https://groups.google.com/forum/#!forum/elm-discuss>"

verifyElmVersion :: V.Version -> ErrorT String IO ()
verifyElmVersion elmVersion =
    if elmVersion == V.elmVersion
        then return ()
        else throwError msg
    where
      msg =
          "elm_dependencies.json says this project depends on version " ++
          V.toString elmVersion ++ " of the compiler but the compiler you " ++
          "have installed is version " ++ V.toString V.elmVersion


verifyExposedModulesExist :: [String] -> ErrorT String IO ()
verifyExposedModulesExist modules =
      mapM_ verifyExists modules
    where
      verifyExists modul =
          let path = Path.moduleToElmFile modul in
          do exists <- liftIO $ doesFileExist path
             when (not exists) $ throwError $
                 "Cannot find module " ++ modul ++ " at " ++ path

verifyMetadata :: Package.Description -> ErrorT String IO ()
verifyMetadata deps =
    case problems of
      [] -> return ()
      _  -> throwError $ "Some of the fields in " ++ P.description ++
                         " have not been filled in yet:\n\n" ++ unlines problems ++
                         "\nFill these in and try to publish again!"
    where
      problems = Maybe.catMaybes
          [ verify Package.repo        "  repository - must refer to a valid repo on GitHub"
          , verify Package.summary     "  summary - a quick summary of your project, 80 characters or less"
          , verify Package.description "  description - extended description, how to get started, any useful references"
          , verify Package.exposed     "  exposed-modules - list modules your project exposes to users"
          ]

      verify what msg =
          if what deps == what Package.defaultDescription
            then Just msg
            else Nothing

verifyVersion :: N.Name -> V.Version -> ErrorT String IO ()
verifyVersion name version =
    do response <- R.versions name
       case response of
         Nothing -> return ()
         Just versions ->
             do let maxVersion = maximum (version:versions)
                when (version < maxVersion) $ throwError $ unlines
                     [ "a later version has already been released."
                     , "Use a version number higher than " ++ V.toString maxVersion ]
                checkSemanticVersioning maxVersion

       checkTag version

    where
      checkSemanticVersioning _ = return ()

      checkTag version = do
        tags <- lines <$> Cmd.git [ "tag", "--list" ]
        let v = V.toString version
        when (v `notElem` tags) $
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