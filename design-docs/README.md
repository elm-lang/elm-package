# Overview

Influences mainly come from `npm` and `opam` (for OCaml)

`elm-get install` can be used in a few ways:

 1. **entirely distributed**: you can just install packages directly from public
    or private github repos. This looks like `elm-get install tom/vector2d`,
    perhaps with a tag or SHA. This is for people who know what they are doing.
 2. **centralized & curated**: I'll create a central listing of "good" libraries.
    You can install these without specifying a user: `elm-get install vector2d`
 3. **private**: This is the mase as (2), but it can be created privately within
    some company's intranet or whatever. The user will add the privately curated hub to
    some local list, in order of priority. So when downloading, `elm-get` will choose
    the first match for a given project. Disambiguate with (1).

# Questions

### How to detect version issues?

The versioning issues can be avoided entirely, but that comes at the cost of size of
generated code. It should be fine to use N different versions of the same library, but
that costs N extra bits.

I'm not sure if working out dependencies should happen during installation
(annoying for everyone, especially beginner/starting project), or if there
should be tools for building things from source later on to minimize
duplication (annoying for professionals). The choice here may have ripple effects
in how people write and maintain libraries.

Either way, converging on a single version of each library will be
necessary eventually. To help with this, `opam` just spins up tons of
VMs to test a bunch of configurations. A simple approximation of this
is to have users report successes and failures when installing.
