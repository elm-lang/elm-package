module Generate.Docs where

import Control.Applicative
import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Exit
import System.FilePath
import System.IO

import qualified Elm.Internal.Dependencies as Deps
import qualified Elm.Internal.Documentation as Docs
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Paths as EPath

import qualified Generate.Elm as Elm
import qualified Generate.Html as Html
import qualified Generate.Listing as Listing
import qualified Utils.Paths as Path

generate :: [Docs.Document] -> Deps.Deps -> FilePath -> ErrorT String IO ()
generate docs deps directory =
  do makeHtml docs deps directory
     liftIO $ Listing.add deps

regenerate :: IO ()
regenerate =
  do listings <- Listing.readListings
     result <- runErrorT $ mapM makeHtml' (concatMap getDirs listings)
     case result of
       Right _ -> return ()
       Left err ->
           do hPutStrLn stderr $ "Failure when regenerating documentation:\n" ++ err
              exitFailure
  where
    getDirs (Listing.Listing name _ vs) =
        map (\version -> Path.libDir </> N.toFilePath name </> show version) vs

    makeHtml' directory = do
      (docs,deps) <- readDocsAndDeps directory
      makeHtml docs deps directory

makeHtml :: [Docs.Document] -> Deps.Deps -> FilePath -> ErrorT String IO Deps.Deps
makeHtml docs deps directory =
  do elms <- Elm.generate docs deps directory
     mapM_ Html.generatePublic elms
     return deps

readDocsAndDeps :: FilePath -> ErrorT String IO ([Docs.Document], Deps.Deps)
readDocsAndDeps directory =
  do docs' <- liftIO $ BS.readFile $ directory </> Path.json
     deps' <- liftIO $ BS.readFile $ directory </> EPath.dependencyFile
     case (,) <$> Json.eitherDecode docs' <*> Json.eitherDecode deps' of
       Left err -> throwError err
       Right result -> return result
