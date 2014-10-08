{-# LANGUAGE OverloadedStrings #-}
module Docs where

import Control.Monad (forM)
import Control.Monad.Error (liftIO)
import qualified Data.ByteString as BS
import qualified Data.List as List

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Package.Description as Desc
import qualified Manager


generate :: Desc.Description -> Manager.Manager FilePath
generate description =
    do  exposedModules <- Desc.locateExposedModules description
        jsonPaths <-
            forM exposedModules $ \(_name, path) -> do
                Cmd.run "elm-doc" [path]
                return (error "path to json docs")

        let packageDocs = error "Path.combinedJson"
        liftIO $ do
            BS.writeFile packageDocs "[\n"
            let addCommas = List.intersperse (BS.appendFile packageDocs ",\n")
            sequence_ $ addCommas $ map (append packageDocs) jsonPaths
            BS.appendFile packageDocs "\n]"

        return packageDocs

    where
      append :: FilePath -> FilePath -> IO ()
      append packageDocs moduleDocs =
        do  json <- BS.readFile moduleDocs
            BS.length json `seq` return ()
            BS.appendFile packageDocs json




