{-# LANGUAGE OverloadedStrings #-}
module Generate.Elm (generate) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import System.FilePath ((<.>), (</>))
import Text.Parsec

import Elm.Internal.Dependencies as Deps
import Elm.Internal.Documentation as Docs
import qualified Elm.Internal.Name as N
import qualified Utils.Paths as Path

generate :: [Document] -> Deps -> FilePath -> ErrorT String IO [FilePath]
generate docs deps directory = (++) <$> makeDocs <*> makeDeps
    where
      makeDocs =
          either throwError (liftIO . mapM (writeDocs directory)) (mapM (docToElm deps) docs)

      makeDeps =
        do liftIO $ writeFile (directory </> Path.index) (depsToElm deps)
           return [directory </> Path.index]

depsToElm :: Deps -> String
depsToElm deps =
    let name = Deps.name deps
        latest = "/catalog/" ++ N.toFilePath name ++ "/latest"
        projectVersion =
            "\"" ++ N.toFilePath name ++ "\" \"" ++ show (Deps.version deps) ++ "\""
    in
    unlines [ "import Website.Skeleton (skeleton)"
            , "import Website.ColorScheme as C"
            , "import Website.Docs.Library as Library"
            , ""
            , "port title : String"
            , "port title = \"" ++ show name ++ "\""
            , ""
            , "links = [ (\"" ++ toLink deps "" ++ "\", toText \"" ++ N.project name ++ "\") ]"
            , ""
            , "main = skeleton links scene (Library.docs " ++ projectVersion ++ ")"
            , ""
            , "scene term docs w ="
            , "  flow down"
            , "  [ color C.mediumGrey <| spacer w 1"
            , "  , width w [markdown|" ++ Deps.description deps ++ "|]"
            , "  , Library.scene " ++ projectVersion ++ " term docs"
            , "  , width w [markdown|The [source code is on GitHub](" ++ Deps.repo deps ++ "),"
            , "so you can star projects, report issues, and follow great library designers."
            , ""
            , "See all previous versions of this library [here](/catalog/" ++ N.toFilePath name ++ ")."
            , ""
            , "Link to the most recent release of this library with: [library.elm-lang.org" ++ latest ++ "](" ++ latest ++ ")|]"
            , "  ]"
            ]

toLink deps m =
    concat [ "/catalog/"
           , N.toFilePath (Deps.name deps)
           , "/", show (Deps.version deps), "/"
           , map (\c -> if c == '.' then '-' else c) m
           ]

writeDocs :: FilePath -> (String,String) -> IO FilePath
writeDocs directory (name, code) =
  do writeFile fileName code
     return fileName
  where
    fileName = directory </> map (\c -> if c == '.' then '-' else c) name <.> "elm"

docToElm :: Deps -> Document -> Either String (String,String)
docToElm deps doc =
  do contents <- either (Left . show) Right $ parse (parseDoc []) name (structure doc)
     case Either.partitionEithers $ map (contentToElm (getEntries doc)) contents of
       ([], code) ->
           Right . (,) name $
           unlines [ "import Website.Skeleton (skeleton)"
                   , "import Website.ColorScheme as C"
                   , "import Website.Docs.Entry (entry)"
                   , "import String"
                   , ""
                   , "links = [ (\"" ++ toLink deps "" ++ "\", toText \"" ++ N.project (Deps.name deps) ++ "\")"
                   , "        , (\"" ++ toLink deps name ++ "\", toText " ++ show name ++ ") ]"
                   , ""
                   , "main = skeleton links scene (constant ())"
                   , ""
                   , "scene term () w ="
                   , "    flow down . map snd . filter (String.contains (String.toLower term) . fst) <|"
                   , "    [ (\"\", color C.mediumGrey <| spacer w 1)"
                   , "    , " ++ List.intercalate "\n    , " code
                   , "    ]"
                   ]
       (missing, _) ->
           Left $ "In module " ++ name ++ ", could not find documentation for: " ++ List.intercalate ", " missing
  where
    name = moduleName doc

    parseDoc contents =
        choice [ eof >> return contents
               , do try (string "@docs")
                    whitespace
                    values <- sepBy1 (var <|> op) comma
                    parseDoc (contents ++ map Value values)
               , do let stop = eof <|> try (string "@docs" >> return ())
                    md <- manyTill anyChar (lookAhead stop)
                    parseDoc (contents ++ [Markdown md])
               ]

    var = (:) <$> letter <*> many (alphaNum <|> oneOf "_'")
    op = do char '(' >> whitespace
            operator <- many1 (satisfy Char.isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")
            whitespace >> char ')'
            return operator

    comma = try (whitespace >> char ',') >> whitespace
    whitespace = many (satisfy (`elem` " \n\r"))

getEntries :: Document -> Map.Map String Entry
getEntries doc =
    Map.fromList $ map (\entry -> (Docs.name entry, entry)) (entries doc)

contentToElm :: Map.Map String Entry -> Content -> Either String String
contentToElm entries content =
    case content of
      Markdown md -> Right $ "(,) \"\" <| width w [markdown|<style>h1,h2,h3,h4 {font-weight:normal;} h1 {font-size:20px;} h2 {font-size:18px;}</style>" ++ md ++ "|]"
      Value name ->
          case Map.lookup name entries of
            Nothing -> Left name
            Just entry ->
                Right $ unwords [ "(,)"
                                , "(String.toLower " ++ show name ++ ")"
                                , "<|"
                                , "entry w"
                                , show name
                                , show (raw entry)
                                , "(" ++ show (assocPrec entry) ++ ")"
                                , "[markdown|" ++ comment entry ++ "|]"
                                ]
