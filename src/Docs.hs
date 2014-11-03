module Docs where

import Control.Monad (forM)
import Control.Monad.Error (liftIO)

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Manager


generate :: Desc.Description -> Manager.Manager FilePath
generate description =
    do  exposedModules <- Desc.locateExposedModules description

        liftIO (writeFile Path.documentation "[\n")

        forM (prep exposedModules) $ \(seperator, path) ->
            do  liftIO (appendFile Path.documentation seperator)
                json <- Cmd.run "elm-doc" [path]
                liftIO (appendFile Path.documentation json)

        liftIO (appendFile Path.documentation "\n]")

        return Path.documentation


prep exposedModules =
    zip ("" : repeat ",\n") (map snd exposedModules)




