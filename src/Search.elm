module Search (box, results) where

import Char
import String
import Http
import Json
import JavaScript.Experimental as JSE
import Graphics.Input as Input
import Dict

(box, searchTerm) = Input.field "  search"

jsonResponse = Http.sendGet (constant "/docs.json")

modules response =
    case response of
      Http.Success str ->
          case Json.fromString str of
            Just (Json.Array xs) ->
                let rawModules = map (JSE.toRecord . Json.toJSObject) xs
                in  Doc (map .name rawModules)
                        (concatMap toValues rawModules)
            _ -> Doc [] []
      _ -> Doc [] []

type Value = { name:String, home:String, tipe:String, description:String }
type Doc = { modules : [String], values : [Value] }

toValues moduleDocs =
    flip map moduleDocs.values <| \v ->
        Value v.name moduleDocs.name v.raw v.comment

contains term string =
    String.contains (String.toLower term) (String.toLower string)

search docs term =
    case term of
      "" -> Dict.empty
      _  -> let dict = foldl (\m d -> Dict.insert m [] d) Dict.empty <|
                       filter (contains term) docs.modules
                insert val dict =
                    case Dict.lookup val.home dict of
                      Nothing -> Dict.insert val.home [val.name] dict
                      Just xs -> Dict.insert val.home (val.name::xs) dict
            in  
              foldl insert dict . reverse <| filter (contains term . .name) docs.values

results = search <~ (modules <~ jsonResponse)
                  ~ searchTerm