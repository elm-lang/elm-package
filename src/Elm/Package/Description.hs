{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Description where

import Prelude hiding (read)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad (when, mzero, forM)
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
import qualified Elm.Package as Package
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Paths as Path
import Elm.Utils ((|>))


data Description = Description
    { name :: Package.Name
    , repo :: String
    , version :: Package.Version
    , elmVersion :: C.Constraint
    , summary :: String
    , license :: String
    , sourceDirs :: [FilePath]
    , exposed :: [Module.Name]
    , natives :: Bool
    , dependencies :: [(Package.Name, C.Constraint)]
    }


defaultDescription :: Description
defaultDescription =
    Description
    { name = Package.Name "USER" "PROJECT"
    , repo = "https://github.com/USER/PROJECT.git"
    , version = Package.initialVersion
    , elmVersion = C.defaultElmVersion
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
  do  json <- liftIO (BS.readFile path)
      case eitherDecode json of
        Left err ->
            throwError $ "Error reading file " ++ path ++ ":\n    " ++ err

        Right ds ->
            return ds


-- WRITE

write :: Description -> IO ()
write description =
    BS.writeFile Path.description json
  where
    json = prettyJSON description


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
    prettyAngles (encodePretty' config description)
  where
    config =
        defConfig { confCompare = keyOrder (normalKeys ++ dependencyKeys) }

    normalKeys =
        [ "version"
        , "summary"
        , "repository"
        , "license"
        , "source-directories"
        , "exposed-modules"
        , "native-modules"
        , "dependencies"
        , "elm-version"
        ]

    dependencyKeys =
        dependencies description
          |> map fst
          |> List.sort
          |> map (T.pack . Package.toString)


prettyAngles :: BS.ByteString -> BS.ByteString
prettyAngles string =
    BS.concat $ replaceChunks string
  where
    replaceChunks str =
        let (before, after) = BS.break (=='\\') str
        in
            case BS.take 6 after of
              "\\u003e" -> before : ">" : replaceChunks (BS.drop 6 after)
              "\\u003c" -> before : "<" : replaceChunks (BS.drop 6 after)
              "" -> [before]
              _ ->
                  before : "\\" : replaceChunks (BS.tail after)


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
        , "elm-version" .= elmVersion d
        ] ++ if natives d then ["native-modules" .= True] else []
    where
      jsonDeps deps =
          Map.fromList $ map (first (T.pack . Package.toString)) deps


instance FromJSON Description where
    parseJSON (Object obj) =
        do  version <- get obj "version" "your project's version number"

            elmVersion <- getElmVersion obj

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

            return $ Description name repo version elmVersion summary license sourceDirs exposed natives deps

    parseJSON _ = mzero



get :: FromJSON a => Object -> T.Text -> String -> Parser a
get obj field desc =
    do maybe <- obj .:? field
       case maybe of
         Just value ->
            return value

         Nothing ->
            fail $
              "Missing field " ++ show field ++ ", " ++ desc ++ ".\n" ++
              "    Check out an example " ++ Path.description ++ " file here:\n" ++
              "    <https://raw.githubusercontent.com/evancz/elm-html/master/elm-package.json>"


getDependencies :: Object -> Parser [(Package.Name, C.Constraint)]
getDependencies obj =
  do  deps <- get obj "dependencies" "a listing of your project's dependencies"
      forM (Map.toList deps) $ \(rawName, rawConstraint) ->
          case Package.fromString rawName of
            Left problem ->
                fail ("Ran into invalid package name '" ++ rawName ++ "' in dependencies.\n\n" ++ problem)

            Right name ->
                case C.fromString rawConstraint of
                    Just constraint ->
                        return (name, constraint)

                    Nothing ->
                        fail (C.errorMessage (Just rawName) rawConstraint)


getElmVersion :: Object -> Parser C.Constraint
getElmVersion obj =
  do  rawConstraint <- get obj "elm-version" elmVersionDescription
      case C.fromString rawConstraint of
        Just constraint ->
            return constraint

        Nothing ->
            fail (C.errorMessage (Just "the elm-version field") rawConstraint)


elmVersionDescription :: String
elmVersionDescription =
  "acceptable versions of the Elm Platform (e.g. \""
  ++ C.toString C.defaultElmVersion ++ "\")"


repoToName :: String -> Either String Package.Name
repoToName rawRepo =
  do  rawName <- dropDomain =<< dropExtension rawRepo
      case Package.fromString rawName of
        Left problem ->
            Left (repoProblem problem)

        Right name ->
            Right name


dropExtension :: String -> Either String String
dropExtension string =
    if List.isSuffixOf ".git" string then
        Right (take (length string - 4) string)

    else
        Left (repoProblem "The given URI does not end with .git")


dropDomain :: String -> Either String String
dropDomain string =
  let
    http = "http://github.com/"
    https = "https://github.com/"
  in
    if List.isPrefixOf http string then
        Right (drop (length http) string)

    else if List.isPrefixOf https string then
        Right (drop (length https) string)

    else
        Left (repoProblem "The given domain does not start with <https://github.com/>")


repoProblem :: String -> String
repoProblem problem =
  unlines
    [ "the 'repository' field must point to a GitHub project, something like"
    , "<https://github.com/USER/PROJECT.git> where USER is your GitHub name and"
    , "PROJECT is the repo you want to upload."
    , ""
    , problem
    ]