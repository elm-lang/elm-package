{-# LANGUAGE FlexibleContexts #-}
module CommandLine.Helpers where

import Control.Monad.Except
import System.Directory
import System.IO

import qualified Elm.Utils as Utils


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
run = Utils.run


out :: (MonadIO m) => String -> m ()
out string =
    liftIO $ hPutStrLn stdout string'
  where
    string' =
        if not (null string) && last string == '\n' then init string else string

