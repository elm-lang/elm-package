module Website.Skeleton (skeleton,home) where

import Graphics.Input as Input
import Website.ColorScheme as C
import Graphics.Input as Input
import Window

headerHeight = 95
footerHeight = 60

(box, searchTerm) = Input.field "  filter"

skeleton : [(String,Text)] -> (String -> a -> Int -> Element) -> Signal a -> Signal Element
skeleton links bodyFunc info =
    lift4 (internalSkeleton links bodyFunc) box searchTerm info Window.dimensions

internalSkeleton links bodyFunc box term info (outer,h) =
    let margin = outer `div` 10
        inner = truncate (toFloat outer * 0.8)
        content = bodyFunc term info (min inner outer)
    in
    color C.lightGrey <|
    flow down
    [ topBar 10 outer
    , container outer headerHeight middle <| flow right
      [ container (inner - widthOf box - 10) headerHeight midLeft <|
        text <| Text.height 30 <| concat <| intersperse (toText " / ") <| (Text.link "/" <| toText "~") ::
        zipWith (<|) (repeat (length links) (uncurry Text.link) ++ [snd]) (("/catalog", toText "Catalog") :: links)
      , container (widthOf box + 10) headerHeight midRight <|
        color C.mediumGrey <|
        container (widthOf box + 2) (heightOf box + 3) middle <|
        color white box
      ]
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - headerHeight - footerHeight)
      in  container outer contentHeight midTop content
    , footer outer
    ]

home bodyFunc = internalHome bodyFunc <~ Window.dimensions

internalHome bodyFunc (outer,h) =
    let margin = outer `div` 10
        inner = truncate (toFloat outer * 0.8)
        content = bodyFunc (min inner outer)
    in
    color C.lightGrey <|
    flow down
    [ topBar 10 outer
    , homeHeader outer inner
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - homeHeaderHeight - footerHeight)
      in  container outer contentHeight midTop content
    , footer outer
    ]

homeHeaderHeight = 160
homeHeader outer inner =
    color (rgb 60 60 60) <| layers
    [ tiledImage outer homeHeaderHeight "/resources/pattern.gif"
    , container outer homeHeaderHeight middle <|
      flow right [ container (inner - 172) homeHeaderHeight pos title
                 , container 172 homeHeaderHeight midLeft <|
                   link "/catalog" <| 
                   color C.mediumGrey <| container 142 62 middle <|
                   color C.accent4 <| container 140 60 middle <|
                   text . Text.height 20 . Text.color C.lightGrey <| toText "Browse"
                 ]
    ]
           
pos = midLeftAt (absolute 30) (relative 0.5)
bigWords = Text.height 50 <| Text.color C.mediumGrey <| toText "Elm Public Library "
alpha = Text.height 20 <| Text.color C.accent3 <| toText "ALPHA"
title =
    flow down
    [ text <| bigWords ++ alpha
    , spacer 10 4
    , text . Text.height 20 . Text.color C.mediumGrey <| toText "Discover libraries, browse documentation"
    ]

accents = [C.accent0,C.accent1,C.accent2,C.accent3,C.accent4]

topBarHeight = 5
topBar k n =
    let n' = toFloat n
        k' = toFloat k
        segs = map (\i -> round (n' * toFloat i / k')) [1..k]
        ws = zipWith (-) segs (0::segs)
        addColors = zipWith color (accents ++ accents)
        box w = spacer w topBarHeight
        boxes = map (\_ -> box) [1..k]
    in  flow right <| addColors (zipWith (<|) boxes ws)

footer outer = container outer footerHeight footerPosition <| Text.centered footerWords
footerPosition = midBottomAt (relative 0.5) (absolute 10)
footerWords =
  let wordLink words1 href words2 words3 =
          toText words1 ++ Text.link href (toText words2) ++ toText words3
  in
     Text.color (rgb 145 145 145) <|
       wordLink "written in Elm and " "https://github.com/evancz/elm-get" "open source" "" ++
       wordLink " / " "https://github.com/evancz" "Evan Czaplicki" " &copy;2013-14"