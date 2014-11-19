module Main where

import System.Directory (findExecutable)
import System.Exit (exitFailure)
import System.IO

import qualified CommandLine.Arguments as Arguments
import qualified Manager


main :: IO ()
main =
  do  requireGit
      manager <- Arguments.parse
      env <- Manager.defaultEnvironment
      result <- Manager.run env manager
      case result of
        Right () ->
            return ()

        Left err ->
            errExit ("\nError: " ++ err ++ newline)
          where
            newline = if last err == '\n' then "" else "\n"


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
