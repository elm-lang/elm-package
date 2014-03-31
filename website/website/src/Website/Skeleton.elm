module Website.Skeleton (skeleton,home) where

import Graphics.Input as Input
import Website.ColorScheme as C
import Graphics.Input as Input
import Window

headerHeight = 80
footerHeight = 60

(box, searchTerm) = Input.field "  filter"

skeleton : [(String,Text)] -> (String -> a -> Int -> Element) -> Signal a -> Signal Element
skeleton links bodyFunc info =
    lift4 (internalSkeleton links bodyFunc) box searchTerm info Window.dimensions

internalSkeleton links bodyFunc box term info (outer,h) =
    let margin = outer `div` 10
        inner = margin * 8
        leftGutter = max margin headerHeight
        content = bodyFunc term info (min inner (outer - leftGutter))
    in
    color C.lightGrey <|
    flow down
    [ topBar outer
    , flow right
      [ container leftGutter headerHeight middle <| link "http://elm-lang.org" <|
        container 50 50 middle <| image 50 50 "/resources/elm_logo_grey.svg"
      , container (inner - widthOf box - 10) headerHeight midLeft <|
        text <| Text.height 30 <| concat <| intersperse (toText " / ") <| (Text.link "/" <| toText "~") ::
        zipWith (<|) (repeat (length links) (uncurry Text.link) ++ [snd]) (("/catalog", toText "Catalog") :: links)
      , container (widthOf box + 10) headerHeight midRight <|
        color C.mediumGrey <|
        container (widthOf box + 2) (heightOf box + 3) middle <|
        color white box
      ]
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - headerHeight - footerHeight)
      in  flow right [ spacer leftGutter contentHeight, content ]
    , footer outer
    ]

home : (Int -> Element) -> Signal Element
home bodyFunc = internalHome bodyFunc <~ Window.dimensions

internalHome : (Int -> Element) -> (Int,Int) -> Element
internalHome bodyFunc (outer,h) =
    let margin = outer `div` 10
        inner = outer - 2 * homeHeaderHeight
        content = bodyFunc inner
    in
    color C.lightGrey <|
    flow down
    [ topBar outer
    , homeHeader outer inner
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - homeHeaderHeight - footerHeight)
      in  container outer contentHeight (topLeftAt (absolute homeHeaderHeight) (absolute 0)) content
    , footer outer
    ]

(logoButton, _) =
    let box c = color c <| container (tileSize) (tileSize) middle <| image 80 80 "/resources/elm_logo_grey.svg"
    in  Input.customButton (box (rgb 57 59 58)) (box C.accent1) (box C.accent1)

tileSize = 84
homeHeaderHeight = 3 * (tileSize `div` 2)
homeHeader outer inner =
    color (rgb 60 60 60) <| layers
    [ tiledImage outer homeHeaderHeight "/resources/tile.png"
    , flow right [ container homeHeaderHeight homeHeaderHeight middle <|
                   link "http://elm-lang.org" logoButton
                 , container (inner - 142) homeHeaderHeight midLeft title
                 , container 142 homeHeaderHeight middle <|
                   link "/catalog" <| 
                   color C.mediumGrey <| container 122 52 middle <|
                   color C.accent1 <| container 120 50 middle <|
                   text . Text.height 20 . Text.color C.lightGrey <| toText "Browse"
                 ]
    ]

bigWords = Text.height 40 <| Text.color C.mediumGrey <| toText "Elm Public Library "
alpha = Text.height 20 <| Text.color C.accent1 <| toText "ALPHA"
title =
    flow down
    [ link "/" <| text <| bigWords ++ alpha
    , spacer 10 4
    , text . Text.height 16 . Text.color C.mediumGrey <| toText "Discover libraries, browse documentation"
    ]

topBarHeight = 6
topBar outer =
    color C.accent1 <| spacer outer topBarHeight

footer outer = container outer footerHeight footerPosition <| Text.centered footerWords
footerPosition = midBottomAt (relative 0.5) (absolute 10)
footerWords =
  let wordLink words1 href words2 words3 =
          toText words1 ++ Text.link href (toText words2) ++ toText words3
  in
     Text.color (rgb 145 145 145) <|
       wordLink "written in Elm and " "https://github.com/elm-lang/elm-get/tree/master/website/src" "open source" "" ++
       wordLink " / " "https://github.com/evancz" "Evan Czaplicki" " &copy;2013-14"