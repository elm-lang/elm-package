module Listing where

import String
import Http
import Json
import JavaScript.Experimental as JSE
import Graphics.Input as Input
import open Website.ColorScheme
import Window

(box, searchTerm) = Input.field "  filter"

jsonResponse = Http.sendGet (constant "/libraries.json")

addSearchText library =
    { library | searchText = String.toLower <| library.name ++ " " ++ library.summary }

format : Http.Response String -> [Library]
format response =
    case response of
      Http.Success str ->
          case Json.fromString str of
            Just (Json.Array xs) ->
                map (addSearchText . JSE.toRecord . Json.toJSObject) xs
            _ -> []
      _ -> []

type Library = { name:String, summary:String, searchText:String, versions:[String] }

search : [Library] -> String -> [Library]
search libraries term =
    let lowTerm = String.toLower term in
    if String.length term < 2 then libraries else
        filter (String.contains lowTerm . .searchText) libraries

libraries : Signal [Library]
libraries = search <~ (format <~ jsonResponse)
                    ~ searchTerm

deslash = String.map (\c -> if c == '/' then '-' else c)

row : Library -> Element
row library =
    flow down
    [ color mediumGrey <| spacer 800 1
    , flow right [ container 200 36 midLeft (text . Text.link ("/libraries/" ++ deslash library.name ++ "/" ++ head library.versions) <| toText library.name)
                 , container 600 36 midLeft (plainText library.summary)
                 ]
    ]

scene (w,h) box libraries =
    let content = flow down
                  [ flow right [ container 400 60 midLeft (text . bold <| toText "Community Libraries")
                               , container 400 60 midRight box ]
                  , flow down <| map row libraries
                  ]
        contentHeight = if heightOf content > h then heightOf content else h
    in  color lightGrey <| container w contentHeight midTop content


main = lift3 scene Window.dimensions box libraries
