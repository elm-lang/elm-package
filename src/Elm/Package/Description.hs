{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Package.Description
  ( Description(..)
  , defaultDescription
  , read, write
  )
  where

import Prelude hiding (read)
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (forM, when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, confCompare, keyOrder)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Data.Text as T

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Package
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Paths as Path
import Elm.Utils ((|>))



-- DESCRIPTION


data Description = Description
    { name :: Package.Name
    , repo :: String
    , version :: Package.Version
    , elmVersion :: C.Constraint
    , summary :: String
    , license :: String
    , sourceDirs :: [FilePath]
    , exposed :: [Module.Raw]
    , natives :: Bool
    , dependencies :: [(Package.Name, C.Constraint)]
    }


defaultDescription :: Description
defaultDescription =
    Description
    { name = Package.Name "user" "project"
    , repo = "https://github.com/user/project.git"
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


read :: (MonadIO m, MonadError e m) => (String -> e) -> FilePath -> m Description
read toError path =
  do  json <- liftIO (BS.readFile path)
      either (throwError . toError) return (decodeDescription json)



-- WRITE


write :: Description -> IO ()
write description =
  BS.writeFile Path.description (prettyJSON description)



-- TO JSON


prettyJSON :: Description -> BS.ByteString
prettyJSON description =
    BS.snoc (prettyAngles (encodePretty' config description)) '\n'
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
    BS.concat $ replaceAngles string


replaceAngles :: BS.ByteString -> [BS.ByteString]
replaceAngles str =
  let
    (before, after) =
      BS.break (=='\\') str
  in
    case BS.take 6 after of
      "\\u003e" ->
        before : ">" : replaceAngles (BS.drop 6 after)

      "\\u003c" ->
        before : "<" : replaceAngles (BS.drop 6 after)

      "" ->
        [before]

      _ ->
        before : "\\" : replaceAngles (BS.tail after)


instance ToJSON Description where
  toJSON d =
      object $
        [ "repository" .= repo d
        , "version" .= version d
        , "summary" .= summary d
        , "license" .= license d
        , "source-directories" .= sourceDirs d
        , "exposed-modules" .= map Module.RawForJson (exposed d)
        , "dependencies" .= jsonDeps (dependencies d)
        , "elm-version" .= elmVersion d
        ] ++ if natives d then ["native-modules" .= True] else []
    where
      jsonDeps deps =
          Map.fromList $ map (first (T.pack . Package.toString)) deps



-- FROM JSON


decodeDescription :: BS.ByteString -> Either String Description
decodeDescription bytestring =
  do  value <- eitherDecode bytestring <|> badJson
      parseEither getDescription value


badJson :: Either String a
badJson =
  Left $
    "I cannot parse the JSON. Maybe a comma is missing? Or there is an extra one?\n\
    \It could also be because of mismatched brackets or quotes.\n\
    \\n\
    \You can also check out the following example to see what it should look like:\n\
    \<https://raw.githubusercontent.com/elm-lang/html/master/elm-package.json>"


instance FromJSON Description where
  parseJSON = getDescription


getDescription :: Value -> Parser Description
getDescription value =
  case value of
    Object obj ->
      do  version <- get obj "version" "your project's version number"

          elmVersion <- getElmVersion obj

          summary <- get obj "summary" "a short summary of your project"
          when (length summary >= 80) $
              fail "The \"summary\" must be less than 80 characters"

          license <- get obj "license" "license information (BSD3 is recommended)"

          repo <- get obj "repository" "a link to the project's GitHub repo"
          name <- case repoToName repo of
                    Left err -> fail err
                    Right nm -> return nm

          exposed <- map Module.fromJson <$> get obj "exposed-modules" "a list of modules exposed to users"

          sourceDirs <- get obj "source-directories" "a list of directories containing source code"

          deps <- getDependencies obj

          natives <- maybe False id <$> obj .:? "native-modules"

          return $ Description name repo version elmVersion summary license sourceDirs exposed natives deps

    _ ->
      fail $
        "I was expecting a JSON object, like the one here:\n\
        \<https://raw.githubusercontent.com/elm-lang/html/master/elm-package.json>"


get :: FromJSON a => Object -> T.Text -> String -> Parser a
get obj field desc =
  do  maybe <- obj .:? field
      case maybe of
        Just value ->
          return value

        Nothing ->
          fail $
            "Missing field " ++ show field ++ " which should hold " ++ desc ++ ".\n\
            \\n\
            \Check out an example " ++ Path.description ++ " file here:\n\
            \<https://raw.githubusercontent.com/elm-lang/html/master/elm-package.json>"


getDependencies :: Object -> Parser [(Package.Name, C.Constraint)]
getDependencies obj =
  do  deps <- get obj "dependencies" "a list of the dependencies you need"
      forM (Map.toList deps) $ \(rawName, rawConstraint) ->
        case Package.fromString rawName of
          Left problem ->
            fail ("Ran into invalid package name \"" ++ rawName ++ "\" in dependencies.\n\n" ++ problem)

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
  "Problem with the \"repository\" field.\n\n" ++ problem
