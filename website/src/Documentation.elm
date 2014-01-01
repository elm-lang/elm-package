import Website.Skeleton (skeleton)
import JavaScript as JS

title = constant (JS.fromString "Library Design Guidelines")
foreign export jsevent "title"
  title : Signal JS.JSString

scene _ () w =
    let words' = width (min w 800) words in
    container w (heightOf words') middle words'

words = [markdown|
<style>
pre {
  background-color: white;
  padding: 10px;
  border: 1px solid rgb(216, 221, 225);
  border-radius: 4px;
}
li { padding: 4px; }
code > span.kw { color: #204a87; font-weight: bold; }
code > span.dt { color: #204a87; }
code > span.dv { color: #0000cf; }
code > span.bn { color: #0000cf; }
code > span.fl { color: #0000cf; }
code > span.ch { color: #4e9a06; }
code > span.st { color: #4e9a06; }
code > span.co { color: #8f5902; font-style: italic; }
code > span.ot { color: #8f5902; }
code > span.al { color: #ef2929; }
code > span.fu { color: #000000; }
code > span.er { font-weight: bold; }
</style>

# Documentation

This documentation format strives for simplicity and regularity. It should
be easy for readers to glance through a file and find the information they
need.

All documentation can use the same markdown as in Elm. You can check out
the [Maybe](https://github.com/evancz/Elm/blob/master/libraries/Maybe.elm)
and [Either](https://github.com/evancz/Elm/blob/master/libraries/Either.elm)
documentation for complete examples.

## Documenting a value

Here is an example from [the `String` library](/catalog/evancz-Elm/0.10.1/String):

```haskell
{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['a','b','c'] == "abc"
-}
fromList : [Char] -> String
fromList = ...
```

Notice that:

  * the documentation starts with `\{-|`
  * the text begins after a single space
  * subsequent lines are aligned with the zeroth column
  * code is indented four spaces
  * there is an example that shows a typical use of the function
  * there is an explicit type annotation

All of these things are necessary. Use this style when documenting your
publicly exposed functions. The goal is to have consistency across all
codebases, so readers can glance through easily and writers do not need
to argue about style.

## Documenting a module

Here is the module documentation for [the `Maybe` library](/catalog/evancz-Elm/0.10.1/Maybe)
in Elm 0.10.1:

```haskell
module Maybe where
{-| Represents an optional value. Maybe it is there, maybe it is not.

# Type and Constructors
@docs Maybe

# Taking Maybes apart
@docs maybe, isJust, isNothing

# Maybes and Lists
@docs justs
-}

import List (foldr)
```

This represents the text that actually gets displayed as [the
documentation](/catalog/evancz-Elm/0.10.1/Maybe) for a module. Notice that:

  * The module documentation comes after the module declaration, but
    before the imports. This is so the first thing in the file is the
    module name and the second is how to use it.
  * The first line starts after a single space, and all subsequent lines
    start in the zeroth column.
  * The `@docs` keyword starts a list of values that are inlined in [the
    resulting documentation](/catalog/evancz-Elm/0.10.1/Maybe).
  * Functions are grouped into related units with titles
  * Although documentation for each function should be self-contained,
    things are ordered intelligently. Assume people will read through
    linearly and try to make the document structure ideal for learning
    the API. You need to understand the Maybe data type to understand
    anything else, so it appears first. `maybe` is an important function
    so it appears early on. Etc.

Again, the goal is to have consistency, so readers can glance through easily
and writers do not need to argue about style.


|]