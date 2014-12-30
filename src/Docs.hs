module Docs where

import Control.Monad (forM)
import Control.Monad.Error (liftIO, throwError)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Manager


generate :: Desc.Description -> Manager.Manager [Docs.Documentation]
generate description =
    do  exposedModules <- Desc.locateExposedModules description

        chunks <-
            forM (prep exposedModules) $ \(seperator, path) ->
                do  json <- Cmd.run "elm-doc" [path]
                    return (seperator ++ json)

        let json = BS.pack (concat chunks ++ "\n]")

        case Json.eitherDecode json of
          Left err ->
            throwError $ "Error generating documentation:\n" ++ err

          Right docs ->
            do  liftIO $ do
                    createDirectoryIfMissing True (dropFileName Path.documentation)
                    BS.writeFile Path.documentation json
                return docs


prep exposedModules =
    zip ("[\n" : repeat ",\n") (map snd exposedModules)




