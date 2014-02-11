{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -W    #-}
module Main where

import Control.Monad.Error
import System.Exit
import System.IO

import qualified Elm.Internal.Name as N

import qualified Get.Install as Install
import Get.Options as Options
import qualified Get.Publish as Publish
import qualified Utils.Commands as Cmd

main :: IO ()
main = do
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
      Install { lib=library, version=maybeVersion } ->
          do name <- N.fromString' library
             Install.install name maybeVersion

      Publish -> Publish.publish

      _ -> do Cmd.out "Not implemented yet!"
              liftIO $ print options
