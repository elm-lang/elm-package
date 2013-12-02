module Website.Skeleton (skeleton,home) where

import Graphics.Input as Input
import Website.ColorScheme as C
import Graphics.Input as Input
import Window

headerHeight = 75
footerHeight = 40

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
    , flow right
      [ spacer margin headerHeight
      , container (inner - widthOf box - 10) headerHeight midLeft <|
        text <| Text.height 22 <| concat <| intersperse (toText " / ") <| (Text.link "/" <| toText "Library") ::
        zipWith (<|) (repeat (length links) (uncurry Text.link) ++ [snd]) (("/catalog", toText "Catalog") :: links)
      , container (widthOf box + 10) headerHeight midRight <|
        color C.mediumGrey <|
        container (widthOf box + 2) (heightOf box + 2) middle <|
        color white box
      ]
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - headerHeight - footerHeight)
      in  container outer contentHeight midTop content
    , container outer footerHeight (midBottomAt (relative 0.5) (absolute 10)) <|
       Text.centered <|
           Text.color (rgb 145 145 145) (toText "&copy; 2011-2013 ") ++
           Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
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
    , let contentHeight = max (heightOf content)
                              (h - topBarHeight - footerHeight)
      in  container outer contentHeight midTop content
    , container outer footerHeight (midBottomAt (relative 0.5) (absolute 10)) <|
       Text.centered <|
           Text.color (rgb 145 145 145) (toText "&copy; 2011-2013 ") ++
           Text.link "https://github.com/evancz" (toText "Evan Czaplicki")
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
