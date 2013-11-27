{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Registry.Generate.Docs where

import Control.Monad.Error
import qualified Registry.Generate.Elm as Elm
import qualified Registry.Generate.Html as Html

generate :: FilePath -> ErrorT String IO ()
generate directory =
  do elms <- Elm.generate directory
     mapM_ Html.generatePublic elms
