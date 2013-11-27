import Docs
import String
import Search
import Window

general = ("General",
  [ "Basics"
  , "Char"
  , "String"
--  , "Regex"
  , "Date"
  ])
containers = ("Containers",
  [ "List"
  , "Dict"
  , "Set"
  , "Maybe"
  , "Either"
  ])

graphics = ("Graphics",
  [ "Graphics.Element"
  , "Graphics.Collage"
  , "Graphics.Input"
  , "Color"
  , "Text"
  , "Transform2D"
  ])

signals = ("Interaction",
  [ "Signal"
  , "Automaton"
  ])
userInput = ("User Input",
  [ "Mouse"
  , "Keyboard"
  , "Touch"
  ])
systemInput = ("System Input",
  [ "Window"
  , "Time"
  , "Random"
  , "Http"
  , "WebSocket"
  ])
ffi = ("JavaScript",
  [ "JavaScript"
  , "Json"
  , "JavaScript.Experimental"
  ])

intro = [markdown|
<style>
h1,h2,h3 { font-weight: normal; }
</style>

This is a central home for discovering, browsing, and learning Elm *libraries*.
If you want to learn the language itself the [syntax reference](http://elm-lang.org/learn/Syntax.elm),
[learning resources](http://elm-lang.org/Learn.elm), and [examples](http://elm-lang.org/Examples.elm)
are all great places to start.

# Standard Libraries

These libraries come with the Elm compiler.
[This table](/InfixOps.elm) shows the relationships between all
standard infix operators.

|]

outro = [markdown|
<style>
h1,h2,h3 { font-weight: normal; }
</style>
<br/>

# Community Libraries

These libraries are available for download on github from
their author or maintainer.

* [elm-d3](https://github.com/seliopou/elm-d3#elm-d3) &ndash; provides Elm
  bindings for [d3.js](http://d3js.org/), effectively making your d3
  code statically typed, pure, and, reactive, and your widgets composable.

|]

linkify name =
    let path = "library/" ++ String.map (\c -> if c == '.' then '/' else c) name ++ ".elm"
    in  Text.toText "    " ++ Text.link path (Text.toText name)

linkList (name, pairs) = 
  flow down . intersperse (spacer 2 2) . map Text.text <|
  Text.height 18 (Text.toText name) :: map linkify pairs

makeCol w = width w . flow down . intersperse (spacer 10 20) . map linkList
threeCol w l m r =
    flow right <| map (makeCol (min 220 <| w `div` 3)) [l,m,r]

col1 = [ general, containers ]
col2 = [ signals, userInput, systemInput ]
col3 = [ graphics, ffi ]

categories =
  [ \w -> width w intro
  , \w -> threeCol w col1 col2 col3
  , \w -> width w outro
  ]

main = Docs.documentation "Library Docs" categories
       <~ Window.dimensions
        ~ Search.box
        ~ Search.results
