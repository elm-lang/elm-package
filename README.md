# elm-package

The package manager for Elm libraries. The full catalog of community libraries
is located at [library.elm-lang.org](http://library.elm-lang.org/).

## Use

The next two sections will cover the basics of using `elm-package` to
install libraries from [the catalog](http://library.elm-lang.org/) and
publish your own.

### Install Libraries

To install a library run:

```bash
elm-package install user/project       # Install latest version
elm-package install user/project 0.1   # Install version 0.1
```

So if you are interested in the
[evancz/automaton](http://library.elm-lang.org/catalog/evancz-automaton/0.1/)
library for Arrowized FRP, you would install it with:

    elm-package install evancz/automaton

`elm-package` is sandboxed by default, so these commands install the
library in the current working directory. This means it is easy for
different projects to have different dependencies. To actually use the
library in your project, you will need to add it to your
`elm_dependencies.json` file which tells the compiler where to look
for extra libraries.

### Publish Libraries

Before publishing, look through the [library design guidelines][guidelines].
Some key takeaways are:

[guidelines]: http://library.elm-lang.org/DesignGuidelines.html

  * Design for a concrete use case
  * Always give functions human readable names
  * Avoid gratuitous abstraction
  * Use [semantic versioning](http://semver.org/)

Use command `elm-package init` for initializing basic `elm_dependencies.json`.
Here are some hints for filling in that information:

  * Keep the `summary` under 80 characters.
  * Make the `description` a useful outline of the library. It should
    cover the problems solved by the library, indicate any
    limitations, and point to related resources that might be helpful.
    The goal is for someone to be able to quickly assess if the
    library will suit their goals.
  * The recommended `license` is BSD3, but of course, you can use
    whatever license you want.
  * The `exposed-modules` are the subset of modules that people can
    use when they install your library. Use this to stop internal
    details from polluting your API and cluttering the docs with
    modules that are not meant for users.
  * For now, you cannot publish libraries with `dependencies` on
    community libraries. It is the highest priority for this project
    to lift this restriction while ensuring that dependency management
    does not become a hellish nightmare.

Once that is all done, use git to tag the specific commit you want to
release with an appropriate version number. This should match the
`version` listed in `elm_dependencies.json`. Finally, you can publish
your library with:

    elm-package publish
