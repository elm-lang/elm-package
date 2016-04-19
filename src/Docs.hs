module Docs (generate) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import qualified Manager



-- GENERATE DOCS / CHECK PERMISSIONS


generate :: Pkg.Name -> Manager.Manager [Docs.Documentation]
generate name =
  do  permissions <- Catalog.permissions name

      let prepublishFlag =
            if permissions then "--prepublish-core" else "--prepublish"

      Cmd.run "elm-make" [ "--yes", "--docs=" ++ Path.documentation, prepublishFlag ]
      json <- liftIO (BS.readFile Path.documentation)

      either badJson return (Json.eitherDecode json)


badJson :: String -> Manager.Manager [Docs.Documentation]
badJson msg =
  throwError $ "Problem with generated documentation:\n" ++ msg
