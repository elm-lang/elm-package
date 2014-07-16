{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Get.Install (install, installAll) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.Writer
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory
import System.FilePath

import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Libraries as L
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Paths as EPath
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Constraint as C

import Get.Dependencies (defaultDeps)
import Get.Library (Library)
import qualified Get.Library as Lib
import qualified Get.Registry as R
import qualified Utils.Commands as Cmd
import qualified Utils.Paths as Path
import Utils.ResolveDeps

-- | Builds up the final transformation on the dependency file using
--   WriterT
type InstallM =
   (WriterT Update -- ^ The updates that need to be run on the deps
    (ErrorT String IO))

execInstallM :: InstallM a -> ErrorT String IO Update
execInstallM = fmap snd . runWriterT

-- | updates to the dependencies
type DepsMap = Map.Map N.Name C.Constraint

-- | Nothing means don't update the file
-- | Just f means apply f to the old dependencies and replace the user's deps file.
newtype Update = Update (Maybe (Endo DepsMap))
                 deriving Monoid

update :: (DepsMap -> DepsMap) -> Update
update = Update . Just . Endo

-- | External Interface
installAll :: ErrorT String IO ()
installAll = installMay Nothing

install :: Library -> ErrorT String IO ()
install = installMay . Just

data InstallFlag = Create
                 | NoCreate
                 | Unknown
                 deriving (Show, Read, Eq, Ord)

installMay :: Maybe Library -> ErrorT String IO ()
installMay mlib =
  do (shouldCreate, deps) <- getDeps `catchError` askCreate
     libs <- toInstall deps
     ups <- execInstallM $ do
              when (shouldCreate == Create) $ tell (update id)
              forM_ libs $ install1 (shouldCreate /= NoCreate) $ D.dependencies deps
     liftIO $ do
       writeUpdates deps ups
       writeLibraries libs
       putStrLn "Success!"
  where
    getDeps =
      do deps <- D.depsAt EPath.dependencyFile
         return (Unknown, deps)

    toInstall :: D.Deps -> ErrorT String IO [Library]
    toInstall deps =
      case mlib of
        Nothing -> fmap (map $ \(n, v) -> Lib.Library n (Just v)) $ solveConstraints deps
        Just _ -> throwError "TODO: implement me"
    
    askCreate _errorMessage =
      do yes <- liftIO $ do
                  putStr createMsg
                  Cmd.yesOrNo
         unless yes . liftIO . putStr $ didntUpdateMsg
         let create = if yes then Create else NoCreate
         return (create, defaultDeps)
      where
        createMsg =
            "Your project does not have a " ++ EPath.dependencyFile ++ " file, which the Elm\n" ++
            "compiler needs to detect dependencies. Should I create it? (y/n): "

writeUpdates :: D.Deps -> Update -> IO ()
writeUpdates deps ups = case applyUpdates deps ups of
  Nothing      -> return ()
  Just newDeps -> BS.writeFile EPath.dependencyFile (D.prettyJSON newDeps)

-- TODO: this Library -> tuple and tuple -> Library conversion should be eliminated
writeLibraries :: [Lib.Library] -> IO ()
writeLibraries libs =
  let libFromTuple (Lib.Library name (Just version)) = Just $ L.Library name version
      libFromTuple _ = Nothing
      libraries = L.Libraries $ Maybe.mapMaybe libFromTuple libs
      json = encode libraries
  in do createDirectoryIfMissing True EPath.dependencyDirectory
        BS.writeFile EPath.librariesFile json

applyUpdates :: D.Deps -> Update -> Maybe D.Deps
applyUpdates d up = (updateDeps . wrapAssoc) (unwrap up) d
  where
    updateDeps upper d = case d of
      D.Deps { D.dependencies = deps } ->
        (\deps' -> d { D.dependencies = deps'}) <$> upper deps

    wrapAssoc :: (Ord k, Functor f) => (Map.Map k v -> f (Map.Map k v)) -> [(k,v)] -> f [(k,v)]
    wrapAssoc upper = fmap Map.toList . upper . Map.fromList

    unwrap :: Update -> DepsMap -> Maybe DepsMap
    unwrap (Update m) d =
      do (Endo f) <- m
         return $ f d

install1 :: Bool -> [(N.Name, C.Constraint)] -> Library -> InstallM ()
install1 shouldAsk oldDeps l@(Lib.Library name _) =
  do finalVsn <- Cmd.inDir EPath.dependencyDirectory $
                 do (repo,version) <- lift $ Cmd.inDir Path.internals $ getRepo l
                    liftIO $ createDirectoryIfMissing True repo
                    Cmd.copyDir (Path.internals </> repo) (repo </> show version)
                    return version

     when shouldAsk $ mkUpdate (Map.fromList oldDeps) name finalVsn
  
mkUpdate :: DepsMap -> N.Name -> V.Version -> InstallM ()
mkUpdate oldDeps n v = case Map.lookup n oldDeps of
  Just c -> case C.satisfyConstraint c v of
    True -> return ()
    False -> liftIO $ putStrLn mismatchMsg
  Nothing ->
    do yes <- shouldI notInstalledAsk
       if yes
         then tell $ update $ Map.insert n (C.Exact v)
         else liftIO $ putStr didntUpdateMsg

  where
    shouldI msg = liftIO $ do putStr msg
                              Cmd.yesOrNo

    notInstalledAsk = "Should I add this library to your " ++ depsFile ++ " file? (y/n): "
    depsFile = EPath.dependencyFile
    mismatchMsg = "Version of library you're installed don't fit into existing range.\n"

      
getRepo :: Library -> ErrorT String IO (FilePath, V.Version)
getRepo l =
  do let directory = N.toFilePath . Lib.lib $ l
     exists  <- liftIO $ doesDirectoryExist directory
     (if exists then update else clone) (Lib.lib l) directory
     version <- getVersion directory l
     Cmd.inDir directory (checkout version)
     return (directory, version)
  where
    update name directory =
      do Cmd.out $ "Getting updates for repo " ++ show name
         Cmd.inDir directory $ do Cmd.git ["checkout", "master"]
                                  Cmd.git ["pull"]
         return ()

    clone name directory =
      do Cmd.out $ "Cloning repo " ++ show name
         Cmd.git [ "clone", "--progress", "https://github.com/" ++ show name ++ ".git" ]
         liftIO $ renameDirectory (N.project name) directory

    checkout version =
        do let tag = show version
           Cmd.out $ "Checking out version " ++ tag
           Cmd.git [ "checkout", "tags/" ++ tag ]

{-| Check to see that the requested version number exists. In the case that no
version number is requested, use the latest tagless version number in the registry.
If the repo is not in the registry, warn the user and check on github.
-}
getVersion :: FilePath -> Library -> ErrorT String IO V.Version
getVersion dir (Lib.Library name mayVsn) =
  do versions <- getVersions name
     case mayVsn of
       Nothing ->
         case filter V.tagless versions of
           [] -> errorNoTags
           vs -> return $ maximum vs
       Just version
         | version `notElem` versions -> errorNoMatch version
         | otherwise                  -> return version
  where
    getVersions :: N.Name -> ErrorT String IO [V.Version]
    getVersions name =
      do registryVersions <- R.versions name
         case registryVersions of
           Just vs -> return vs
           Nothing ->
             do Cmd.out $ "Warning: library " ++ show name ++ " is not registered publicly. Checking github..."
                tags <- lines <$> (Cmd.inDir dir . Cmd.git $ [ "tag", "--list" ])
                Cmd.out $ unlines tags
                return $ Maybe.mapMaybe V.fromString tags

    errorNoTags =
      throwError $ unlines
        [ "did not find any properly tagged releases of this library."
        , "Libraries have at least one tag (like 0.1.2 or 1.0) to ensure that your build"
        , "process is stable and repeatable. These tags should follow Semantic Versioning."
        ]

    errorNoMatch version =
      throwError $ "could not find version " ++ show version ++ " on github."

didntUpdateMsg :: String
didntUpdateMsg =
    "Okay, but if you decide to make this library visible to the compiler later, add\n\
    \the dependency to your " ++ EPath.dependencyFile ++ " file."
