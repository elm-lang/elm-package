module Install.Solver where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson (decode, FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Network.HTTP.Client (responseBody, httpLbs)
import System.FilePath ((</>))
import qualified System.Directory as Dir

import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as S
import qualified Elm.Package.Version as V
import qualified Get.Registry as Reg
import qualified Utils.Http as Http
import qualified Utils.Cache as Cache


solve :: [(N.Name, C.Constraint)] -> ErrorT String IO S.Solution
solve constraints =
    error "solveConstraints: not audited yet"
{--
  do libraryDb <- readLibraries
     let name = Package.name description
         version = Package.version description
         unreader = runReaderT (solveForVersion name version) $
                    SolverEnv libraryDb readDependencies
         initialState = SolverState M.empty
     (solved, _) <- runStateT unreader initialState
     let result = M.delete (Package.name description) solved
     return (M.toList result)

{--
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
--}


data LibraryInfo = LibraryInfo
    { name :: String
    , versions :: [V.Version]
    }


type LibraryDB = Map String LibraryInfo
type Constraints = [(N.Name, Constraint.Constraint)]

buildMap :: Ord k => (v -> k) -> [v] -> Map k v
buildMap key values = foldl' (\map v -> M.insert (key v) v map) M.empty values

-- | For every minor version remove all patch versions but last
onlyLastPatches :: LibraryInfo -> LibraryInfo
onlyLastPatches info = info { versions = process $ versions info }
  where
    process ls =
      let insert v@(V.Version _ _ _) = error "TODO" --M.insertWith (++) (take 2 parts) [v]
          allByMinor = foldr insert M.empty ls
      in map maximum $ M.elems allByMinor

-- | Read information about libraries, probably from local cache
readLibraries :: ErrorT String IO LibraryDB
readLibraries =
  let dir = Path.packagesDirectory </> "_elm_get_cache"
      fileName = "libraries.json"
      downloadAction = decodeFromUrl $ Reg.domain ++ "/libraries.json"
  in
  do ls <- cacheWrapper downloadAction dir fileName
     return $ buildMap name $ map onlyLastPatches ls

readDependencies :: N.Name -> V.Version -> ErrorT String IO Constraints
readDependencies name version =
    Package.dependencies <$> cacheWrapper downloadAction dir fileName
  where
    fullUrl =
        concat
        [ Reg.domain , "/catalog/"
        , N.toFilePath name
        , "/", V.toString version
        , "/", Path.description
        ]

    dir =
        Path.packagesDirectory </> "_elm_get_cache" </> N.toFilePath name
    
    fileName = V.toString version ++ ".json"
    
    downloadAction = decodeFromUrl fullUrl


data SolverState = SolverState
    { ssLibrariesMap :: Map (N.Name, V.Version) Constraints
    }

{-| Configuration of solver, which stays constant for every launch.

Constists of:
* information about all available libraries and their versions
* function to read dependencies and their constraints for particular
  version of particular library
-}
data SolverEnv m = SolverEnv
    { libraryDb :: LibraryDB
    , readDepsFunction :: N.Name -> V.Version -> ErrorT String m Constraints
    }

type SolverContext m =
  ReaderT (SolverEnv m) -- information about libraries, deps-fetching function
  (StateT SolverState   -- solver current state, also RAM-cached dependencies info
   (ErrorT String m))   -- underlying effects for IO and errors

getConstraints :: Monad m => N.Name -> V.Version -> SolverContext m Constraints
getConstraints name version =
  do libsMap <- gets ssLibrariesMap
     case M.lookup (name, version) libsMap of
       Just deps -> return deps
       Nothing ->
         do readDeps <- asks readDepsFunction
            deps <- lift . lift $ readDeps name version
            modify (\s -> s { ssLibrariesMap = M.insert (name, version) deps libsMap })
            return deps

type PinnedLibs = Map N.Name V.Version
type ConstrainedLibs = Map N.Name [V.Version]


addConstraints :: Monad m => ConstrainedLibs -> Constraints -> SolverContext m (Maybe ConstrainedLibs)
addConstraints constrained constraints =
    foldM addConstraint (Just constrained) constraints
  where
    addConstraint curr (name, constraint) =
      case curr of
        Nothing -> return Nothing
        Just values ->
          do versions <- getValidVersions name values
             case filter (Constraint.isSatisfied constraint) versions of
               [] -> return Nothing
               ls -> return $ Just $ M.insert name ls values

    getValidVersions name constrained =
      case M.lookup name constrained of
        Just vs -> return vs
        Nothing ->
          do libs <- asks libraryDb
             case versions <$> M.lookup (N.toString name) libs of
               Nothing -> throwError (notFound name)
               Just vs -> return vs

    notFound name =
        "Versions of library " ++ N.toString name ++ " weren't found"


solve :: Monad m => PinnedLibs -> ConstrainedLibs -> SolverContext m (Maybe PinnedLibs)
solve fixed constrained =
  case M.minViewWithKey constrained of
    Nothing -> return $ Just fixed
    Just ((name, versions), rest) -> foldM tryEvery Nothing versions
      where
        tryEvery currSolution version =
          case currSolution of
            Just _ -> return currSolution
            Nothing -> tryNext version

        satisfyFixed (n, c) =
          case M.lookup n fixed of
            Just v -> Constraint.isSatisfied c v
            Nothing -> False

        tryNext version =
          do constraints <- getConstraints name version
             let (existCs, newCs) = span (\(n, _) -> M.member n fixed) constraints
             newConstrained <- addConstraints rest newCs
             case (all satisfyFixed existCs, newConstrained) of
               (True, Just value) -> solve (M.insert name version fixed) value
               _ -> return Nothing

solveForVersion :: Monad m => N.Name -> V.Version -> SolverContext m (Map N.Name V.Version)
solveForVersion name version =
  do solution <- solve M.empty (M.singleton name [version])
     case solution of
       Just value -> return value
       Nothing -> throwError "Solving dependencies failed"

--}