module Home where

import String
import Website.ColorScheme as C
import Website.Skeleton (home)
import Window

scene w = width w [markdown|
<style>h1,h2,h3,h4 {font-weight:normal;}</style>

# Elm Public Library

This is a central home for discovering, browsing, and learning Elm libraries.

## <a href="/catalog">catalog</a>

|]

main = home scene
