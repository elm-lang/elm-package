module Website.Docs.Library (docs,scene) where

import Dict
import Http
import Json
import JavaScript.Experimental as JSE
import Maybe
import String

-- Extract library info from JSON

docs : Signal Doc
docs = extract <~ Http.sendGet (constant "docs.json")

extract : Http.Response String -> Doc
extract response =
    case response of
      Http.Success str ->
          case Json.fromString str of
            Just (Json.Array xs) ->
                let rawModules = map (JSE.toRecord . Json.toJSObject) xs
                in  Doc (map .name rawModules) (concatMap toValues rawModules)
            _ -> Doc [] []
      _ -> Doc [] []

type Value = { name:String, home:String }
type Doc = { modules : [String], values : [Value] }

toValues modul =
    let home = modul.name
        toValue v = Value v.name home
    in
        concat [ map toValue modul.values
               , map toValue modul.datatypes
               , map toValue modul.aliases
               ]

-- Search for library info

search : Doc -> String -> Dict.Dict String [String]
search doc term =
    case term of
      "" -> Dict.fromList <| map (\m -> (m,[])) doc.modules
      _  -> let dict = foldl (\m d -> Dict.insert m [] d) Dict.empty <|
                       filter (contains term) doc.modules
                insert value dict =
                    Dict.update value.home (updater value.name) dict
            in  
                foldr insert dict <| filter (contains term . .name) doc.values

contains : String -> String -> Bool
contains a b =
    String.contains (String.toLower a) (String.toLower b)

updater : a -> Maybe [a] -> Maybe [a]
updater x maybe =
    Just (x :: Maybe.maybe [] id maybe)

-- Show the search results

scene : String -> Doc -> Element
scene term doc =
    let results = flow down . map showModule . Dict.toList <| search doc term
    in  spacer 20 4 `beside` results

showModule : (String, [String]) -> Element
showModule (name, values) =
    let results = flow down <| map (valueLink name) values
    in
      flow down [ moduleLink name
                , results
                , spacer 10 (if isEmpty values then 6 else 14) ]

moduleLink : String -> Element
moduleLink name =
    text . Text.link (toPath name) <| toText name

valueLink : String -> String -> Element
valueLink modul name =
    let address = toPath modul ++ "#" ++ name
    in  flow down [ spacer 10 4
                  , spacer 20 4 `beside` (text . monospace . Text.link address <| toText name)
                  ]

toPath : String -> String
toPath = String.map (\c -> if c == '.' then '-' else c)

