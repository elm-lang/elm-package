module Docs where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Docs as Docs
import qualified Elm.Package.Paths as Path
import qualified Manager


generate :: Manager.Manager [Docs.Documentation]
generate =
  do  Cmd.run "elm-make" ["--yes", "--docs=" ++ Path.documentation]
      json <- liftIO (BS.readFile Path.documentation)

      either badJson return (Json.eitherDecode json)
  where
    badJson :: String -> Manager.Manager [Docs.Documentation]
    badJson msg =
      throwError $ "Problem with generated documentation:\n" ++ msg
