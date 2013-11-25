module Docs where

import Dict
import Window
import String
import open ColorScheme

documentation name es (w,h) search results =
    let title wid = container wid 100 middle . text . Text.height 60 <| toText name
        searchBar = sideBar search results
        content = flow down (map (\e -> e (w-240)) (title :: es))
        maxH = maximum <| h :: map heightOf [searchBar, content]
    in  
      flow right [ height maxH searchBar
                 , color mediumGrey <| spacer 1 maxH
                 , spacer 19 maxH
                 , content
                 , spacer 20 maxH ]

border e = color mediumGrey <| container (widthOf e + 2) (heightOf e + 2) middle e

toPath = String.map (\c -> if c == '.' then '/' else c)

moduleLink w str =
    let address = "/library/" ++ toPath str ++ ".elm"
    in  text . Text.link address <| toText str

valueLink w home value =
    let address = "/library/" ++ toPath home ++ ".elm#" ++ value
    in  spacer 10 1 `beside` (width (w-10) . text . monospace . Text.link address <| toText value)

showModule w (name, values) =
    flow down . intersperse (spacer w 4) <|
    moduleLink w name :: map (valueLink w name) values ++ [spacer w 16]

tabs =
  let tabHeight = 36
      tab (name, href) =
          let words = text . Text.height 12 . Text.link href <| toText name
          in  container (widthOf words + 10) tabHeight midTop words
  in  container 200 tabHeight middle . flow right <| map tab
          [ ("Learn"    , "http://elm-lang.org/Learn.elm")
          , ("Examples" , "http://elm-lang.org/Examples.elm")
          , ("Docs"     , "/")
          , ("Install"  , "http://elm-lang.org/Install.elm")
          ]

sideBar search results =
    color lightGrey <|
    flow down [ link "http://elm-lang.org" <| fittedImage 200 80 "/tangram-logo.png"
              , tabs
              , container 200 50 middle . border <| width 170 search
              , spacer 200 20
              , spacer 20 10 `beside`
                (flow down . map (showModule 160) <| Dict.toList results)
              ]

entry : String -> String -> Maybe (String,Int) -> Element -> Int -> Element
entry name tipe assocPrec prose w =
    let box n pos txt = container w (heightOf txt + n) pos txt
        ap = case assocPrec of
               Nothing -> []
               Just (a,p) -> [ box 2 topRight . text . monospace . toText <|
                                   a ++ "-associative, precedence " ++ show p ++ " " ]
        tipe' = box 2 topLeft . text <| monospace (toText " ") ++ prettify tipe
    in
      flow down [ tag name . color mediumGrey <| spacer w 1
                , color lightGrey . layers <| ap ++ [ tipe' ]
                , flow right [ spacer 40 10, width (w-40) prose ]
                , spacer w 4
                ]

until c xs =
  case String.uncons xs of
    Nothing -> ("","")
    Just (hd,tl) ->
        if | hd == '(' -> let (chomped,rest) = until ')' tl
                              (before,after) = until c rest
                          in  (String.cons hd chomped ++ before, after)
           | hd == c -> ("", xs)
           | otherwise -> let (before,after) = until c tl
                          in  (String.cons hd before, after)

prettify raw =
    if | String.startsWith "type " raw || String.startsWith "data " raw ->
           let (name, rest) = until ' ' (String.sub 5 (String.length raw) raw) in
           monospace <| concat [ Text.color accent1 <| toText (String.sub 0 5 raw)
                               , bold <| toText name
                               , colorize "" rest
                               ]
       | otherwise ->
           let (name, rest) = until ':' raw
           in  monospace <| bold (toText name) ++ colorize "" rest

colorize stuff str =
  let continue clr op rest =
          toText (String.reverse stuff) ++ Text.color clr (toText op) ++ colorize "" rest
  in 
  case String.uncons str of
    Nothing -> toText (String.reverse stuff)
    Just (c,rest) ->
        if | c == ':' -> continue accent1 ":" rest
           | c == '|' -> continue accent1 "|" rest
           | c == '=' -> continue accent1 "=" rest
           | c == '-' -> case String.uncons rest of
                           Just ('>',rest') -> continue accent1 "->" rest'
                           _ -> colorize (String.cons c stuff) rest
           | otherwise -> colorize (String.cons c stuff) rest
