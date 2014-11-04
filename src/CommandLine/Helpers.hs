{-# LANGUAGE FlexibleContexts #-}
module CommandLine.Helpers where

import Control.Monad.Error
import System.Directory
import System.Exit
import System.IO
import System.Process


yesOrNo :: IO Bool
yesOrNo =
  do  hFlush stdout
      input <- getLine
      case input of
        "y" -> return True
        "n" -> return False
        _   -> do putStr "Must type 'y' for yes or 'n' for no: "
                  yesOrNo


inDir :: (MonadError String m, MonadIO m) => FilePath -> m a -> m a
inDir dir doStuff =
  do  here <- liftIO $ getCurrentDirectory
      liftIO $ createDirectoryIfMissing True dir
      liftIO $ setCurrentDirectory dir
      result <- doStuff
      liftIO $ setCurrentDirectory here
      return result


git :: (MonadError String m, MonadIO m) => [String] -> m String
git = run "git"


run :: (MonadError String m, MonadIO m) => String -> [String] -> m String
run command args =
  do  result <- liftIO runCommand
      case result of
        Right out -> return out
        Left err ->
            throwError $
            "failure when running:" ++ concatMap (' ':) (command:args) ++ "\n" ++ err
  where
    runCommand =
        do  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
            return $ case exitCode of
                       ExitSuccess -> Right stdout
                       ExitFailure code
                           | code == 127  -> Left missingExe  -- UNIX
                           | code == 9009 -> Left missingExe  -- Windows
                           | otherwise    -> Left stderr

    missingExe =
        "Could not find command '" ++ command ++ "'. Do you have it installed?\n\
        \    Can it be run from anywhere? Is it on your PATH?"


out :: (MonadIO m) => String -> m ()
out string =
    liftIO $ hPutStrLn stdout string'
  where
    string' =
        if not (null string) && last string == '\n' then init string else string

