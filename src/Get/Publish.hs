{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Get.Publish where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.Aeson
import GHC.Generics
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Clock
import System.Directory
import System.Exit
import System.IO

import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.SolvedDependencies as SD
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Assets as A
import qualified Elm.Internal.Version as V

import Get.Dependencies (defaultDeps)
import qualified Get.Registry as R
import qualified Utils.Commands as Cmd
import qualified Utils.Http as Http
import qualified Utils.Paths as Path
import qualified Utils.SemverCheck as Semver

data SavedMetadata = SavedMetadata
  { baseVersion :: V.Version
  , nextVersion :: V.Version
  , apiCompatibility :: Semver.Compatibility
  } deriving (Generic)

instance ToJSON SavedMetadata
instance FromJSON SavedMetadata

savedMetadataFilename :: String
savedMetadataFilename = "publish_in_progress.json"

savedMetadataDiffTime :: IO (Maybe Integer)
savedMetadataDiffTime =
  do exist <- doesFileExist savedMetadataFilename
     case exist of
       False -> return Nothing
       True ->
         do modTime <- getModificationTime savedMetadataFilename
            currTime <- Clock.getCurrentTime
            return $ Just $ floor (Clock.diffUTCTime currTime modTime)

publish :: ErrorT String IO ()
publish =
  do diffTime <- liftIO savedMetadataDiffTime
     case diffTime of
       Just seconds
         | seconds < 0 -> throwError "Negative modification time!"
         | seconds < 10 * 60 -> publishStep2
         | otherwise ->
           do liftIO $ putStrLn $ savedMetadataFilename ++ " is outdated!"
              publishStep1
       Nothing -> publishStep1

publishStep1 :: ErrorT String IO ()
publishStep1 =
  do deps <- getDeps
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyElmVersion (D.elmVersion deps)
     verifyMetadata deps
     verifyExposedModulesExist exposedModules
     verifyVersionExist name version
     generateDocs exposedModules
     docsComparison <- compareDocs name version
     let compat = Semver.compatibility docsComparison
         bump = Semver.bumpByCompatibility compat
         newVersion = Semver.bumpVersion bump version
     continue <- lift $ proposeVersion compat bump newVersion
     when continue $
       do let metadata = SavedMetadata { baseVersion = version
                                       , nextVersion = newVersion
                                       , apiCompatibility = compat
                                       }
          liftIO $ BSL.writeFile savedMetadataFilename $ encode metadata

publishStep2 :: ErrorT String IO ()
publishStep2 =
  do liftIO $ putStrLn "Continuing publish process..."

exitAtFail :: ErrorT String IO a -> ErrorT String IO a
exitAtFail action =
  do either <- liftIO $ runErrorT $ action
     case either of
       Right deps -> return deps
       Left err ->
           liftIO $ do hPutStrLn stderr $ "\nError: " ++ err
                       exitFailure

getDeps :: ErrorT String IO D.Deps
getDeps = exitAtFail $ D.depsAt A.dependencyFile

getVersions :: ErrorT String IO [(N.Name, V.Version)]
getVersions = exitAtFail $ SD.read A.solvedDependencies

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
                     "have installed is version " ++ show V.elmVersion
    where
      V.V ns' _ = V.elmVersion

verifyExposedModulesExist :: [String] -> ErrorT String IO ()
verifyExposedModulesExist modules =
      mapM_ verifyExists modules
    where
      verifyExists modul =
          let path = Path.moduleToElmFile modul in
          do exists <- liftIO $ doesFileExist path
             when (not exists) $ throwError $
                 "Cannot find module " ++ modul ++ " at " ++ path

verifyMetadata :: D.Deps -> ErrorT String IO ()
verifyMetadata deps =
    case problems of
      [] -> return ()
      _  -> throwError $ "Some of the fields in " ++ A.dependencyFile ++
                         " have not been filled in yet:\n\n" ++ unlines problems ++
                         "\nFill these in and try to publish again!"
    where
      problems = Maybe.catMaybes
          [ verify D.repo        "  repository - must refer to a valid repo on GitHub"
          , verify D.summary     "  summary - a quick summary of your project, 80 characters or less"
          , verify D.description "  description - extended description, how to get started, any useful references"
          , verify D.exposed     "  exposed-modules - list modules your project exposes to users"
          ]

      verify what msg =
          if what deps == what defaultDeps
            then Just msg
            else Nothing

verifyVersionExist :: N.Name -> V.Version -> ErrorT String IO ()
verifyVersionExist name version =
  do response <- R.versions name
     case response of
       Nothing -> return ()
       Just versions ->
          when (not $ version `elem` versions) $ throwError $ unlines
            [ "base version doesn't exist"
            , "Version number in your " ++ A.dependencyFile ++ " should be for base (previous) version"
            , "not resulting version number"
            ]

generateDocs :: [String] -> ErrorT String IO ()
generateDocs modules =
    do Cmd.run "elm" ("--generate-docs" : "--make" : elms)
       liftIO $
         do let path = Path.combinedJson
            BS.writeFile path "[\n"
            let addCommas = List.intersperse (BS.appendFile path ",\n")
            sequence_ $ addCommas $ map append jsons
            BS.appendFile path "\n]"

    where
      elms = map Path.moduleToElmFile modules
      jsons = map Path.moduleToJsonFile modules

      append :: FilePath -> IO ()
      append path =
        do json <- BS.readFile path
           BS.length json `seq` return ()
           BS.appendFile Path.combinedJson json

errorFromMaybe :: String -> Maybe a -> ErrorT String IO a
errorFromMaybe err = Maybe.maybe (throwError err) return

compareDocs :: N.Name -> V.Version -> ErrorT String IO Semver.DocsComparison
compareDocs name version =
  let url = concat [ R.domain, "/catalog/", N.toFilePath name, "/"
                   , V.toString version, "/docs.json"]
  in
  do mv1 <- liftIO $ decodeStrict <$> BS.readFile Path.combinedJson
     v1 <- errorFromMaybe "Parse error while reading local docs.json" mv1
     v2 <- Http.decodeFromUrl url

     case AT.parseEither Semver.buildDocsComparison (v1, v2) of
       Left err -> throwError err
       Right result -> return result

proposeVersion :: Semver.Compatibility -> Semver.IndexPos -> V.Version -> IO Bool
proposeVersion compat bump newVersion =
  do putStr compatMessage
     putStr "Proceed? [y/n]: "
     Cmd.yesOrNo
  where
    showCompatibility c =
      case c of
        Semver.Same -> "is the same as"
        Semver.Compatible -> "is compatible to"
        Semver.Incompatible -> "isn't compatible to"

    compatMessage =
      unlines
      [ "Based on automatic comparison, your current API " ++ showCompatibility compat
      , "API of base version. Therefore, by semantic versioning, " ++ Semver.showIndexPos bump
      , "should be increased."
      , ""
      , "Proposed version: " ++ V.toString newVersion
      ]
