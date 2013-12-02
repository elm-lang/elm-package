module Registry.Generate.Docs where

import Control.Applicative
import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Registry.Generate.Elm as Elm
import qualified Registry.Generate.Html as Html
import qualified Registry.Generate.Listing as Listing
import qualified Registry.Utils as Utils
import qualified Get.Utils as GUtils
import System.FilePath

generate :: FilePath -> ErrorT String IO ()
generate directory =
  do docs' <- liftIO $ BS.readFile $ directory </> Utils.json
     deps' <- liftIO $ BS.readFile $ directory </> GUtils.depsFile
     case (,) <$> Json.eitherDecode docs' <*> Json.eitherDecode deps' of
       Left err -> throwError err
       Right (docs,deps) ->
           do elms <- Elm.generate docs deps directory
              mapM_ Html.generatePublic elms
              liftIO $ Listing.add deps
