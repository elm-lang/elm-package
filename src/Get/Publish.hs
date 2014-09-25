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

import qualified Package.Description as Package
import qualified Package.Dependencies as Deps
import qualified Package.Name as N
import qualified Package.Paths as P
import qualified Package.Version as V

import qualified Get.Registry as R
import qualified Utils.Commands as Cmd
import qualified Utils.Http as Http
import qualified Utils.Paths as Path
import qualified Utils.SemverCheck as Semver

data SavedMetadata = SavedMetadata
  { baseVersion :: V.Version
  , nextVersion :: V.Version
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
     let name = Package.name deps
         version = Package.version deps
         exposedModules = Package.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyElmVersion (Package.elmVersion deps)
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
                                       }
          liftIO $ BSL.writeFile savedMetadataFilename $ encode metadata
          liftIO $ BSL.writeFile P.description $ Package.prettyJSON (deps { Package.version = newVersion })

checkMetadata :: SavedMetadata -> Package.Description -> ErrorT String IO ()
checkMetadata metadata deps =
  do assert "Version wasn't modified between launches" $ Package.version deps == nextVersion metadata
     let version = baseVersion metadata
         name = Package.name deps
     docsComparison <- compareDocs name version
     let compat = Semver.compatibility docsComparison
         bump = Semver.bumpByCompatibility compat
         newVersion = Semver.bumpVersion bump version
     assert "Version number wasn't compromised" $ newVersion == nextVersion metadata

  where
    assert msg assertion =
      case assertion of
        False -> throwError $
                 unlines [ "Assertion failed:"
                         , msg
                         , ""
                         , "It appears you made unallowed changes to " ++ P.description
                         , "Easiest way to continue publishing package is to remove"
                         , savedMetadataFilename ++ " and restore base version in " ++ P.description
                         ]
        True -> return ()

publishStep2 :: ErrorT String IO ()
publishStep2 =
  do liftIO $ putStrLn "Continuing publish process..."
     maybeMetadata <- liftIO $ decode <$> BSL.readFile savedMetadataFilename
     metadata <- errorFromMaybe (savedMetadataFilename ++ " is malformed!") maybeMetadata
     deps <- getDeps
     checkMetadata metadata deps
     R.register (Package.name deps) (Package.version deps) Path.combinedJson
     Cmd.out "Success!"

exitAtFail :: ErrorT String IO a -> ErrorT String IO a
exitAtFail action =
  do either <- liftIO $ runErrorT $ action
     case either of
       Right deps -> return deps
       Left err ->
           liftIO $ do hPutStrLn stderr $ "\nError: " ++ err
                       exitFailure

getDeps :: ErrorT String IO Package.Description
getDeps =
    exitAtFail $ Package.descriptionAt P.description

getVersions :: ErrorT String IO [(N.Name, V.Version)]
getVersions =
    exitAtFail $ Deps.read P.dependencies

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

verifyVersionExist :: N.Name -> V.Version -> ErrorT String IO ()
verifyVersionExist name version =
  do response <- R.versions name
     case response of
       Nothing -> return ()
       Just versions ->
          when (not $ version `elem` versions) $ throwError $ unlines
            [ "base version doesn't exist"
            , "Version number in your " ++ P.description ++ " should be for base (previous) version"
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
