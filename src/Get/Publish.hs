{-# LANGUAGE OverloadedStrings #-}
module Get.Publish where

import Control.Applicative ((<$>))
import Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory
import System.Exit
import System.IO
import Text.JSON

import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Paths as EPath
import qualified Elm.Internal.Version as V

import qualified Get.Registry as R
import qualified Utils.Commands as Cmd
import qualified Utils.Paths as Path
import qualified Utils.PrettyJson as Pretty

publish :: ErrorT String IO ()
publish =
  do deps <- getDeps
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyNoDependencies (D.dependencies deps)
     verifyElmVersion (D.elmVersion deps)
     verifyExposedModules exposedModules
     verifyVersion name version
     withCleanup $ do
       generateDocs exposedModules
       R.register name version Path.combinedJson
     Cmd.out "Success!"

getDeps :: ErrorT String IO D.Deps
getDeps =
  do either <- liftIO $ runErrorT $ D.depsAt EPath.dependencyFile
     case either of
       Right deps -> return deps
       Left err ->
           liftIO $ do
             hPutStrLn stderr $ "\nError: " ++ err
             hPutStr stdout $ "\nWould you like me to add the missing fields? (y/n) "
             yes <- Cmd.yesOrNo
             case yes of
               False -> hPutStrLn stdout "Okay, maybe next time!"
               True -> do
                 addMissing =<< readFields
                 hPutStrLn stdout $ "Done! Now go through " ++ EPath.dependencyFile ++
                      " and check that\neach field is filled in with valid and helpful information."
             exitFailure

addMissing :: Map.Map String JSValue -> IO ()
addMissing existingFields =
    writeFile EPath.dependencyFile $ show $ Pretty.object obj'
    where
      obj' = map (\(f,v) -> (f, Maybe.fromMaybe v (Map.lookup f existingFields))) obj

      str = JSString . toJSString
      obj = [ ("version", str "0.1")
            , ("summary", str "concise, helpful summary of your project")
            , ("description", str "full description of this project, describe your use case")
            , ("license", str "BSD3")
            , ("repository", str "https://github.com/USER/PROJECT.git")
            , ("exposed-modules", JSArray [])
            , ("elm-version", str $ show V.elmVersion)
            , ("dependencies", JSObject $ toJSObject [])
            ]

readFields :: IO (Map.Map String JSValue)
readFields =
    do exists <- doesFileExist EPath.dependencyFile
       case exists of
         False -> return Map.empty
         True -> do raw <- readFile EPath.dependencyFile
                    return $ case decode raw of
                      Error _ -> Map.empty
                      Ok obj  -> Map.fromList $ fromJSObject obj

withCleanup :: ErrorT String IO () -> ErrorT String IO ()
withCleanup action =
    do existed <- liftIO $ doesDirectoryExist "docs"
       either <- liftIO $ runErrorT action
       when (not existed) $ liftIO $ removeDirectoryRecursive "docs"
       case either of
         Left err -> throwError err
         Right () -> return ()

verifyNoDependencies :: [(N.Name,V.Version)] -> ErrorT String IO ()
verifyNoDependencies [] = return ()
verifyNoDependencies _ =
    throwError
        "elm-get is not able to publish projects with dependencies\n\
        \yet. This is obviously a very high proirity, and I am working as\n\
        \fast as I can! For now, let people know about your library on the\n\
        \mailing list: <https://groups.google.com/forum/#!forum/elm-discuss>"

verifyElmVersion :: V.Version -> ErrorT String IO ()
verifyElmVersion elmVersion@(V.V ns _)
    | ns == ns' = return ()
    | otherwise =
        throwError $ "elm_dependencies.json says this project depends on version " ++
                     show elmVersion ++ " of the compiler but the compiler you " ++
                     "have installed is version " ++ show V.elmVersion
    where
      V.V ns' _ = V.elmVersion

verifyExposedModules :: [String] -> ErrorT String IO ()
verifyExposedModules modules =
    do when (null modules) $ throwError $
              "There are no exposed modules in " ++ EPath.dependencyFile ++
              "!\nAll libraries must make at least one module available to users."
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
