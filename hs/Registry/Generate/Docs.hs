module Registry.Generate.Docs where

import Control.Applicative
import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import qualified Registry.Generate.Elm as Elm
import qualified Registry.Generate.Html as Html
import qualified Registry.Generate.Listing as Listing
import qualified Registry.Utils as Utils
import qualified Get.Utils as GUtils
import qualified Model.Dependencies as D
import qualified Model.Name as N
import System.FilePath
import System.IO
import System.Exit

generate :: FilePath -> ErrorT String IO ()
generate directory =
  do deps <- makeHtml directory
     liftIO $ Listing.add deps

regenerate :: IO ()
regenerate =
  do listings <- Listing.readListings
     result <- runErrorT $ do
                 dirs <- concat <$> mapM getDirs (Map.elems listings)
                 mapM makeHtml dirs
     case result of
       Right _ -> return ()
       Left err ->
           do hPutStrLn stderr $ "Failure when regenerating documentation:\n" ++ err
              exitFailure
  where
    getDirs (Listing.Listing name _ vs) =
        do project <- N.toFilePath <$> N.fromString' name
           return $ map (\version -> Utils.libDir </> project </> version) vs

makeHtml :: FilePath -> ErrorT String IO D.Deps
makeHtml directory =
  do docs' <- liftIO $ BS.readFile $ directory </> Utils.json
     deps' <- liftIO $ BS.readFile $ directory </> GUtils.depsFile
     case (,) <$> Json.eitherDecode docs' <*> Json.eitherDecode deps' of
       Left err -> throwError err
       Right (docs,deps) ->
           do elms <- Elm.generate docs deps directory
              mapM_ Html.generatePublic elms
              return deps
