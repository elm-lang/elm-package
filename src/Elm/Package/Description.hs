{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Description where

import Prelude hiding (read)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.Error (MonadError, throwError, MonadIO, liftIO, when, mzero, forM)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, confCompare, keyOrder)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Paths as Path


data Description = Description
    { name :: N.Name
    , repo :: String
    , version :: V.Version
    , summary :: String
    , license :: String
    , sourceDirs :: [FilePath]
    , exposed :: [Module.Name]
    , natives :: Bool
    , dependencies :: [(N.Name, C.Constraint)]
    }


defaultDescription :: Description
defaultDescription =
    Description
    { name = N.Name "USER" "PROJECT"
    , repo = "https://github.com/USER/PROJECT.git"
    , version = V.initialVersion
    , summary = "helpful summary of your project, less than 80 characters"
    , license = "BSD3"
    , sourceDirs = [ "." ]
    , exposed = []
    , natives = False
    , dependencies = []
    }


-- READ

read :: (MonadIO m, MonadError String m) => FilePath -> m Description
read path =
    do json <- liftIO (BS.readFile path)
       case eitherDecode json of
         Left err -> throwError $ "Error reading file " ++ path ++ ":\n    " ++ err
         Right ds -> return ds


-- WRITE

write :: Description -> IO ()
write description =
    BS.writeFile Path.description json
  where
    json = prettyAngles (prettyJSON description)


prettyAngles :: BS.ByteString -> BS.ByteString
prettyAngles string =
    BS.concat $ replaceChunks string
  where
    replaceChunks str =
        let (before, after) = BS.break (=='\\') str
        in
            case BS.take 5 after of
              "\\u003e" -> before : ">" : replaceChunks (BS.drop 5 after)
              "\\u003c" -> before : "<" : replaceChunks (BS.drop 5 after)
              "" -> [before]
              _ ->
                  before : "\\" : replaceChunks (BS.tail after)


-- FIND MODULE FILE PATHS

locateExposedModules :: (MonadIO m, MonadError String m) => Description -> m [(Module.Name, FilePath)]
locateExposedModules desc =
    mapM locate (exposed desc)
  where
    locate modul =
      let path = Module.nameToPath modul <.> "elm"
          dirs = sourceDirs desc
      in
      do  possibleLocations <-
              forM dirs $ \dir -> do
                  exists <- liftIO $ doesFileExist (dir </> path)
                  return (if exists then Just (dir </> path) else Nothing)

          case Maybe.catMaybes possibleLocations of
            [] ->
                throwError $
                unlines
                [ "Could not find exposed module '" ++ Module.nameToString modul ++ "' when looking through"
                , "the following source directories:"
                , concatMap ("\n    " ++) dirs
                , ""
                , "You may need to add a source directory to your " ++ Path.description ++ " file."
                ]

            [location] ->
                return (modul, location)

            locations ->
                throwError $
                unlines
                [ "I found more than one module named '" ++ Module.nameToString modul ++ "' in the"
                , "following locations:"
                , concatMap ("\n    " ++) locations
                , ""
                , "Module names must be unique within your package."
                ]


-- JSON

prettyJSON :: Description -> BS.ByteString
prettyJSON description =
    encodePretty' config description
  where
    config = defConfig { confCompare = order }
    order =
        keyOrder
        [ "version"
        , "summary"
        , "repository"
        , "license"
        , "source-directories"
        , "exposed-modules"
        , "native-modules"
        , "dependencies"
        ]


instance ToJSON Description where
  toJSON d =
      object $
      [ "repository" .= repo d
      , "version" .= version d
      , "summary" .= summary d
      , "license" .= license d
      , "source-directories" .= sourceDirs d
      , "exposed-modules" .= exposed d
      , "dependencies" .= jsonDeps (dependencies d)
      ] ++ if natives d then ["native-modules" .= True] else []
    where
      jsonDeps deps =
          Map.fromList $ map (first (T.pack . N.toString)) deps


instance FromJSON Description where
    parseJSON (Object obj) =
        do  version <- get obj "version" "your projects version number"

            summary <- get obj "summary" "a short summary of your project"
            when (length summary >= 80) $
                fail "'summary' must be less than 80 characters"

            license <- get obj "license" "license information (BSD3 is recommended)"

            repo <- get obj "repository" "a link to the project's GitHub repo"
            name <- case repoToName repo of
                      Left err -> fail err
                      Right nm -> return nm

            exposed <- get obj "exposed-modules" "a list of modules exposed to users"

            sourceDirs <- get obj "source-directories" "the directories that hold source code"

            deps <- getDependencies obj

            natives <- maybe False id <$> obj .:? "native-modules"

            return $ Description name repo version summary license sourceDirs exposed natives deps

    parseJSON _ = mzero



get :: FromJSON a => Object -> T.Text -> String -> Parser a
get obj field desc =
    do maybe <- obj .:? field
       case maybe of
         Just value -> return value
         Nothing -> fail $ "Missing field " ++ show field ++ ", " ++ desc ++ ".\n" ++
                           "    Check out an example " ++ Path.description ++ " file here:" ++
                           "    <https://github.com/evancz/elm-html/blob/master/elm_dependencies.json>"


getDependencies :: Object -> Parser [(N.Name, C.Constraint)]
getDependencies obj =
    toDeps =<< get obj "dependencies" "a listing of your project's dependencies"
  where
    toDeps deps =
        forM (Map.toList deps) $ \(f, c) ->
            case (N.fromString f, C.fromString c) of
              (Just name, Just constr) -> return (name, constr)
              (Nothing, _) -> fail $ N.errorMsg f
              (_, Nothing) -> fail $ C.errorMessage c


repoToName :: String -> Either String N.Name
repoToName repo =
    if not (end `List.isSuffixOf` repo)
        then Left msg
        else
            do  path <- getPath
                let raw = take (length path - length end) path
                case N.fromString raw of
                  Nothing   -> Left msg
                  Just name -> Right name
    where
      getPath
          | http  `List.isPrefixOf` repo = Right $ drop (length http ) repo
          | https `List.isPrefixOf` repo = Right $ drop (length https) repo
          | otherwise = Left msg

      http  = "http://github.com/"
      https = "https://github.com/"
      end = ".git"
      msg =
          "the 'repository' field must point to a GitHub project for now, something\n\
          \like <https://github.com/USER/PROJECT.git> where USER is your GitHub name\n\
          \and PROJECT is the repo you want to upload."