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

It's conceivable that there will be breaking changes in future
releases, so it may be necessary to clear out the catalog
at some point for the long-term health of `elm-get`.

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
look through the [Library Design Guidelines](/DesignGuidelines.html). Some
key takeaways are:

 * Design for a concrete use case
 * Always give functions human readable names
 * Avoid gratuitous abstraction
 * Use [semantic versioning](http://semver.org)

After looking through [the guidelines](/DesignGuidelines.html) carefully,
publish your library with:

```
elm-get publish
```

|]

main = home scene
