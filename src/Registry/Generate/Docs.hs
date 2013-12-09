module Registry.Generate.Docs where

import Control.Applicative
import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import qualified Registry.Generate.Elm as Elm
import qualified Registry.Generate.Html as Html
import qualified Registry.Generate.Listing as Listing
import qualified Utils.Paths as Path
import qualified Utils.Model.Dependencies as D
import qualified Utils.Model.Name as N
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
           return $ map (\version -> Path.libDir </> project </> version) vs

makeHtml :: FilePath -> ErrorT String IO D.Deps
makeHtml directory =
  do docs' <- liftIO $ BS.readFile $ directory </> Path.json
     deps' <- liftIO $ BS.readFile $ directory </> Path.depsFile
     case (,) <$> Json.eitherDecode docs' <*> Json.eitherDecode deps' of
       Left err -> throwError err
       Right (docs,deps) ->
           do elms <- Elm.generate docs deps directory
              mapM_ Html.generatePublic elms
              return deps
