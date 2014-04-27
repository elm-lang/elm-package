{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -W    #-}
module Main where

import Control.Applicative
import Control.Monad.Error
import Data.Version (showVersion)
import System.Directory (findExecutable)
import System.Exit
import System.IO

import qualified Elm.Internal.Name as N

import qualified Get.Install as Install
import Get.Library
import Get.Options as Options
import qualified Get.Publish as Publish
import qualified Paths_elm_get as This
import qualified Utils.Commands as Cmd

main :: IO ()
main = do
  gitCheck
  cmd <- parse
  result <- runErrorT (handle cmd)
  case result of
    Right _ -> return ()
    Left err ->
        do hPutStr stderr ("\nError: " ++ err ++ newline)
           exitFailure
        where
          newline = if last err == '\n' then "" else "\n"

handle :: Command -> ErrorT String IO ()
handle options =
  case options of
    Version ->
      liftIO $ putStrLn $ "elm-get " ++ showVersion This.version
    Install mLib ->
      Install.install =<< (updateMaybe . updateName) N.fromString' mLib

    Publish -> Publish.publish

    Update _ -> Cmd.out "Not implemented yet!"
  where
    updateMaybe :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
    updateMaybe up m =
        case m of
          Nothing -> pure Nothing
          Just x  -> Just <$> up x

gitCheck :: IO ()
gitCheck =
  do maybePath <- findExecutable "git"
     case maybePath of
       Just _  -> return ()
       Nothing ->
           do hPutStrLn stderr gitNotInstalledMessage
              exitFailure
  where
    gitNotInstalledMessage =
        "\n\
        \The REPL relies on git to download libraries and manage versions.\n\
        \    It appears that you do not have git installed though!\n\
        \    Get it from <http://git-scm.com/downloads> to continue."
