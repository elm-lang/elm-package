module Main where

import Control.Monad.Error
import System.Directory (findExecutable)
import System.Exit
import System.IO

import qualified Install as Install
import qualified Options as Options
import qualified Publish as Publish


main :: IO ()
main =
  do  requireGit
      command <- Options.parse
      result <- runErrorT (run command)
      case result of
        Right () -> return ()
        Left err ->
            do hPutStr stderr ("\nError: " ++ err ++ newline)
               exitFailure
            where
              newline = if last err == '\n' then "" else "\n"


run :: Options.Command -> ErrorT String IO ()
run command =
    case command of
      Options.Install maybePackage ->
          Install.install maybePackage

      Options.Publish ->
          Publish.publish


errExit :: String -> IO ()
errExit msg =
  do  hPutStrLn stderr msg
      exitFailure


requireGit :: IO ()
requireGit =
  do  maybePath <- findExecutable "git"
      case maybePath of
        Just _  -> return ()
        Nothing -> errExit gitNotInstalledMessage
  where
    gitNotInstalledMessage =
        "\n\
        \The REPL relies on git to download libraries and manage versions.\n\
        \    It appears that you do not have git installed though!\n\
        \    Get it from <http://git-scm.com/downloads> to continue."
