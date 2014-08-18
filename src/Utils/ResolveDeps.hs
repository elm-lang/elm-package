{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Utils.ResolveDeps where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson (decode, FromJSON, ToJSON, encode)
import Data.Functor ((<$>))
import Data.List (foldl')
import Data.Map (Map)
import GHC.Generics (Generic)
import Network.HTTP.Client (responseBody, httpLbs)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified System.Directory as Dir

import qualified Elm.Internal.Constraint as C
import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Assets as A
import qualified Elm.Internal.Version as V
import qualified Get.Registry as Reg
import qualified Utils.Http as Http
import qualified Utils.Cache as Cache

-- | Try to read a JSON-encoded value from a file
decodeFromFile :: (MonadIO m, FromJSON a) => FilePath -> m (Maybe a)
decodeFromFile fullPath =
  do fileExists <- liftIO $ Dir.doesFileExist fullPath
     case fileExists of
       False -> return Nothing
       True ->
         do fileContents <- liftIO $ BS.readFile fullPath
            case decode fileContents of
              Nothing ->
                do liftIO $ putStrLn $ "Cache contents at " ++ fullPath ++ " are malformed"
                   return Nothing
              result@Just{} -> return result

-- | Try to write a value to a file in particular directory, encoding it to JSON
encodeToFile :: ToJSON a => a -> FilePath -> FilePath -> IO ()
encodeToFile value dir fileName =
  do Dir.createDirectoryIfMissing True dir
     BS.writeFile (dir </> fileName) (encode value)

-- | A wrapper around Utils.Cache.cacheComputation to simplify implementing
--   caching for downloaded packages metadata
cacheWrapper :: (FromJSON a, ToJSON a, MonadIO m) => m a -> FilePath -> FilePath -> m a
cacheWrapper uncached dir fileName =
  let readSomething =
        do let fullPath = dir </> fileName
           decodeFromFile fullPath
      writeSomething x = encodeToFile x dir fileName
  in Cache.cacheComputation
     uncached
     (liftIO $ readSomething)
     (\x -> liftIO $ writeSomething x)

-- | Try to read a JSON-encoded value from an URL. Throws an error in case of parse error
decodeFromUrl :: FromJSON a => String -> ErrorT String IO a
decodeFromUrl url =
  do result <- Http.send url $ \request manager ->
       fmap (decode . responseBody) $ httpLbs request manager
     case result of
       Just v -> return v
       Nothing -> throwError $ "Can't read value from " ++ url

-- | Library description as used in library.elm-lang.org/libraries.json
data LibraryInfo = LibraryInfo
    { name :: String
    , summary :: String
    , versions :: [V.Version]
    } deriving (Show, Generic)

instance FromJSON LibraryInfo
instance ToJSON LibraryInfo

type LibraryDB = Map String LibraryInfo
type Constraints = [(N.Name, C.Constraint)]

buildMap :: Ord k => (v -> k) -> [v] -> Map k v
buildMap key values = foldl' (\map v -> M.insert (key v) v map) M.empty values

-- | For every minor version remove all patch versions but last
onlyLastPatches :: LibraryInfo -> LibraryInfo
onlyLastPatches info = info { versions = process $ versions info }
  where
    process ls =
      let insert v@(V.V parts _) = M.insertWith (++) (take 2 parts) [v]
          allByMinor = foldr insert M.empty ls
      in map maximum $ M.elems allByMinor

-- | Read information about libraries, probably from local cache
readLibraries :: ErrorT String IO LibraryDB
readLibraries =
  let dir = A.packagesDirectory </> "_elm_get_cache"
      fileName = "libraries.json"
      downloadAction = decodeFromUrl $ Reg.domain ++ "/libraries.json"
  in
  do ls <- cacheWrapper downloadAction dir fileName
     return $ buildMap name $ map onlyLastPatches ls

readDependencies :: N.Name -> V.Version -> ErrorT String IO Constraints
readDependencies name version =
  let fullUrl = concat [ Reg.domain , "/catalog/"
                       , N.toFilePath name
                       , "/", show version
                       , "/", A.dependencyFile
                       ]
      dir = A.packagesDirectory </> "_elm_get_cache" </> N.toFilePath name
      fileName = show version ++ ".json"
      downloadAction = decodeFromUrl fullUrl
  in D.dependencies <$> cacheWrapper downloadAction dir fileName

data SolverState = SolverState
    { ssLibrariesMap :: Map (N.Name, V.Version) Constraints
    }

{-| Configuration of solver, which stays constant for every launch.

Constists of:
* information about all available libraries and their versions
* function to read dependencies and their constraints for particular
  version of particular library
-}
data SolverEnv = SolverEnv
    { libraryDb :: LibraryDB
    , readDepsFunction :: N.Name -> V.Version -> ErrorT String IO Constraints
    }

type SolverContext =
  ReaderT SolverEnv    -- information about libraries, deps-fetching function
  (StateT SolverState  -- solver current state, also RAM-cached dependencies info
   (ErrorT String IO)) -- underlying effects for IO and errors

getConstraints :: N.Name -> V.Version -> SolverContext Constraints
getConstraints name version =
  do libsMap <- gets ssLibrariesMap
     case M.lookup (name, version) libsMap of
       Just deps -> return deps
       Nothing ->
         do readDeps <- asks readDepsFunction
            deps <- lift . lift $ readDeps name version
            modify (\s -> s { ssLibrariesMap = M.insert (name, version) deps libsMap })
            return deps

addConstraints :: Map N.Name [V.Version] -> [(N.Name, C.Constraint)] -> SolverContext (Maybe (Map N.Name [V.Version]))
addConstraints constrained constraints = foldM addConstraint (Just constrained) constraints
  where
    addConstraint :: Maybe (Map N.Name [V.Version]) -> (N.Name, C.Constraint) -> SolverContext (Maybe (Map N.Name [V.Version]))
    addConstraint curr (name, constraint) =
      case curr of
        Nothing -> return Nothing
        Just values ->
          do versions <-
               case M.lookup name values of
                 Just vs -> return vs
                 Nothing ->
                   do libs <- asks libraryDb
                      case versions <$> M.lookup (N.toString name) libs of
                        Nothing -> throwError $ "Versions of library " ++ N.toString name ++ " weren't found"
                        Just vs -> return vs
             case filter (C.satisfyConstraint constraint) versions of
               [] -> return Nothing
               ls -> return $ Just $ M.insert name ls values

solve :: Map N.Name V.Version -> Map N.Name [V.Version] -> SolverContext (Maybe (Map N.Name V.Version))
solve fixed constrained =
  case M.minViewWithKey constrained of
    Nothing -> return $ Just $ fixed
    Just ((name, versions), rest) -> foldM tryEvery Nothing versions
      where
        tryEvery currSolution version =
          case currSolution of
            Just _ -> return currSolution
            Nothing ->
              do constraints <- getConstraints name version
                 newConstrained <- addConstraints rest constraints
                 case newConstrained of
                   Nothing -> return Nothing
                   Just value -> solve (M.insert name version fixed) value

solveForVersion :: N.Name -> V.Version -> SolverContext (Map N.Name V.Version)
solveForVersion name version =
  do solution <- solve M.empty (M.singleton name [version])
     case solution of
       Just value -> return value
       Nothing -> throwError "Can't solve a thing :-("

solveConstraints :: D.Deps -> ErrorT String IO [(N.Name, V.Version)]
solveConstraints deps =
  do libraryDb <- readLibraries
     let name = D.name deps
         version = D.version deps
         unreader = runReaderT (solveForVersion name version) $
                    SolverEnv libraryDb readDependencies
         initialState = SolverState M.empty
     (solved, _) <- runStateT unreader initialState
     let result = M.delete (D.name deps) solved
     return (M.toList result)
