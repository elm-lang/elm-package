# elm-get

A command line tool to share Elm libraries.
The full catalog of Elm libraries lives at
[library.elm-lang.org](http://library.elm-lang.org/).

## Install

    cabal install elm-get

This will install the `elm-get` executable in `/home/YOU/.cabal/bin`.

### Use

#### Install

To install a library run:

```bash
elm-get install user/project       # Install latest version
elm-get install user/project 0.1   # Install version 0.1
```

So if you are interested in the
[evancz/automaton](http://library.elm-lang.org/catalog/evancz-automaton/0.1/)
library for Arrowized FRP, you would install it with `elm-get install evancz/automaton`.

`elm-get` is sandboxed by default, so these commands install the
library in the current working directory. This means it is easy for
different projects to have different dependencies.

To actually use the library in your project, you will need to add it
to your `elm_dependencies.json` file which tells the compiler where
to look for extra libraries.

#### Publish

To publish, run the following command from the root of your project:

    elm-get publish
