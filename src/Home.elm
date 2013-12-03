module Home where

import String
import Website.ColorScheme as C
import Website.Skeleton (home)
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm Public Library")
foreign export jsevent "title"
  title : Signal JS.JSString

padCol w col =
    let col' = col (w-60) in
    container w (heightOf col') middle col'

scene w =
    flow down 
    [ spacer w 20
    , flow right [ padCol (div w 2) leftCol
                 , padCol (div w 2) rightCol ]
    ]

--The Public Library is the central catalog of libraries created by the Elm community.
--It makes it easy to [discover libraries, browse documentation, and share ideas](/catalog).

leftCol w = width w [markdown|
<style>
h1,h2,h3,h4 {font-weight:normal;}
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
</style>

# Install Libraries

Use [`elm-get`](https://github.com/evancz/elm-get) to install libraries:

```
elm-get install user/project
```

[`elm-get`](https://github.com/evancz/elm-get) is sandboxed by default,
installing on a per-project basis. It is also backed by [GitHub](https://github.com/),
so you can use it to install any public repo.

# Warning

<span style="color:#cc0000;">
**Do not post publicly about this site!**
</span> Everything is pre-release. The goal is
to find and [report](https://github.com/evancz/elm-get)
bugs in the library servers and `elm-get` before a broader
public announcement.

The catalog will be cleared before the official release,
so please experiment with publishing and try to find corner cases!

|]

rightCol w = width w [markdown|
<style>
h1,h2,h3,h4 {font-weight:normal;}
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
li { padding: 4px; }
</style>

# Design Guidelines

Before publishing libraries with [`elm-get`](https://github.com/evancz/elm-get),
it is important to look through these basic guidelines:

 * Design libraries for a concrete use case
 * Always give functions human readable names
 * Avoid abbreviations and infix operators
 * Avoid abstraction for the sake of abstraction
 * Always use [semantic versioning](http://semver.org)

After reviewing your library, publish it with:

```
elm-get publish
```

|]

main = home scene
