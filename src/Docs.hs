{-# LANGUAGE OverloadedStrings #-}
module Docs where

import Control.Monad (forM)
import Control.Monad.Error (throwError, liftIO)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Manager


generate :: Desc.Description -> Manager.Manager FilePath
generate description =
    do  exposedModules <- locateExposedModules description
        jsonPaths <-
            forM exposedModules $ \path -> do
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


locateExposedModules :: Desc.Description -> Manager.Manager [FilePath]
locateExposedModules desc =
    mapM locate (Desc.exposed desc)
  where
    locate modul =
      let path = moduleToElmFile modul
          dirs = Desc.sourceDirs desc
      in
      do  possibleLocations <-
              forM dirs $ \dir -> do
                  exists <- liftIO $ doesFileExist (dir </> path)
                  return (if exists then Just (dir </> path) else Nothing)

          case Maybe.catMaybes possibleLocations of
            [] ->
                throwError $
                unlines
                [ "Could not find exposed module '" ++ modul ++ "' when looking through"
                , "the following source directories:"
                , concatMap ("\n    " ++) dirs
                , ""
                , "You may need to add a source directory to your " ++ Path.description ++ " file."
                ]

            [location] ->
                return location

            locations ->
                throwError $
                unlines
                [ "I found more than one module named '" ++ modul ++ "' in the"
                , "following locations:"
                , concatMap ("\n    " ++) locations
                , ""
                , "Module names must be unique within your package."
                ]


moduleToElmFile :: String -> FilePath
moduleToElmFile moduleName =
    swapDots moduleName ++ ".elm"


swapDots :: String -> String
swapDots =
    map (\c -> if c == '.' then '/' else c)


