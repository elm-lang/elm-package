module Get.Install (install) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Text.JSON

import qualified Get.Registry as R
import qualified Utils.Paths as Path
import qualified Utils.Commands as Cmd
import qualified Elm.Internal.Paths as EPath
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import qualified Utils.PrettyJson as Pretty

install :: N.Name -> Maybe String -> ErrorT String IO ()
install name maybeVersion =
    do version <-
           Cmd.inDir EPath.dependencyDirectory $ do
             (repo,version) <- Cmd.inDir Path.internals (get name maybeVersion)
             liftIO $ createDirectoryIfMissing True repo
             Cmd.copyDir (Path.internals </> repo) (repo </> show version)
             return version
       liftIO $ addToDepsFile name version
       Cmd.out "Success!"

get :: N.Name -> Maybe String -> ErrorT String IO (FilePath, V.Version)
get name maybeVersion =
  do exists <- liftIO $ doesDirectoryExist directory
     if exists then update else clone
     version <- getVersion name maybeVersion
     Cmd.inDir directory (checkout version)
     return (directory, version)
  where
    directory = N.toFilePath name

    update = do
      Cmd.out $ "Getting updates for repo " ++ show name
      Cmd.inDir directory $ do Cmd.git ["checkout", "master"]
                               Cmd.git ["pull"]
      return ()

    clone = do
      Cmd.out $ "Cloning repo " ++ show name
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
getVersion :: N.Name -> Maybe String -> ErrorT String IO V.Version
getVersion name maybeVersion' =
    do maybeVersion <- validateVersion maybeVersion'
       versions <- getVersions name
       case maybeVersion of
         Nothing ->
             case filter V.tagless versions of
               [] -> errorNoTags
               vs -> return $ maximum vs
         Just version
             | version `notElem` versions -> errorNoMatch version
             | otherwise                  -> return version
    where
      validateVersion :: Maybe String -> ErrorT String IO (Maybe V.Version)
      validateVersion version =
          case (version, V.fromString =<< version) of
            (Just tag, Nothing) ->
                throwError $ unlines $
                [ "tag " ++ tag ++ " is not a valid version number."
                , "It must have the following format: 0.1.2 or 0.1.2-tag"
                ]
            (_, result) -> return result

      getVersions :: N.Name -> ErrorT String IO [V.Version]
      getVersions name = do
        registryVersions <- R.versions name
        case registryVersions of
          Just vs -> return vs
          Nothing -> do
            Cmd.out $ "Warning: library " ++ show name ++
                      " is not registered publicly. Checking github..."
            tags <- lines <$> Cmd.git [ "tag", "--list" ]
            return $ Maybe.mapMaybe V.fromString tags

      errorNoTags =
          throwError $ unlines 
          [ "did not find any properly tagged releases of this library."
          , "Libraries have at least one tag (like 0.1.2 or 1.0) to ensure that your build"
          , "process is stable and repeatable. These tags should follow Semantic Versioning."
          ]

      errorNoMatch version =
          throwError $ unlines
          [ "could not find version " ++ show version ++ " on github."
          ]

addToDepsFile :: N.Name -> V.Version -> IO ()
addToDepsFile name version =
    do exists <- doesFileExist file
       add (if exists then yesFile else noFile)
    where
      file = EPath.dependencyFile

      add msg = do
        hPutStr stdout $ msg ++ " (y/n): "
        yes <- Cmd.yesOrNo
        if yes then writeFile file =<< newDependencies name version
               else hPutStrLn stdout oddChoice

      oddChoice =
          "Okay, but if you decide to make this library visible to the compiler\n\
          \later, add the dependency to your " ++ file ++ " file."

      yesFile = "Should I add this library to your " ++ file ++ " file?"
      noFile =
        concat
        [ "Your project does not have a " ++ file ++ " file yet.\n"
        , "Should I create it and add the library you just installed?" ]

newDependencies :: N.Name -> V.Version -> IO String
newDependencies name version =
    do exists <- doesFileExist EPath.dependencyFile
       raw <- if not exists then return "{}" else
                  withFile EPath.dependencyFile ReadMode $ \handle ->
                      do stuff <- hGetContents handle
                         length stuff `seq` return stuff
       case decode raw of
         Error msg -> do
           hPutStrLn stderr $ "Error reading " ++ EPath.dependencyFile ++ ":\n" ++ msg
           exitFailure
         Ok obj ->
             let assocs = fromJSObject obj in
             case List.lookup "dependencies" assocs of
               Just (JSObject entries) -> do
                 entries' <- updateEntries (fromJSObject entries)
                 return $ addDeps assocs entries'
               _ -> return $ addDeps assocs [entry]

    where
      entry = (show name, JSString $ toJSString $ show version)

      addDeps assocs entries = show $ Pretty.object obj
          where
            assocs' = filter ((/=) "dependencies" . fst) assocs
            obj = assocs' ++ [("dependencies", JSObject $ toJSObject entries)]

      updateEntries ::[(String,JSValue)] -> IO [(String,JSValue)]
      updateEntries entries =
          let name' = show name
              entries' = List.insertBy (compare `on` fst) entry $
                         filter ((/=) name' . fst) entries
          in
          case List.lookup name' entries of
            Just (JSString oldVersion) -> do
              hPutStr stdout $
                 name' ++ " " ++ fromJSString oldVersion ++ " is already in " ++
                 EPath.dependencyFile ++ ".\nDo you want to replace it " ++
                 "with version " ++ show version ++ "? (y/n): "
              yes <- Cmd.yesOrNo
              case yes of
                True -> return entries'
                False -> hPutStrLn stdout msg >> return entries
                    where msg = "Okay, but be sure to change the version number if\n\
                                \you want to use the library you just installed."

            _ -> return entries'
