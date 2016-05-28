module CommandLine.Helpers where

import Control.Monad.Except (liftIO, runExceptT, throwError)
import System.Directory
import System.IO

import qualified Elm.Utils as Utils
import qualified Manager
import qualified Reporting.Error as Error


yesOrNo :: IO Bool
yesOrNo =
  do  hFlush stdout
      input <- getLine
      case input of
        ""  -> return True
        "Y" -> return True
        "y" -> return True
        "n" -> return False
        _   ->
          do  putStr "Must type 'y' for yes or 'n' for no: "
              yesOrNo


inDir :: FilePath -> Manager.Manager a -> Manager.Manager a
inDir dir task =
  do  here <- liftIO $ getCurrentDirectory
      liftIO $ createDirectoryIfMissing True dir
      liftIO $ setCurrentDirectory dir
      result <- task
      liftIO $ setCurrentDirectory here
      return result


run :: String -> [String] -> Manager.Manager String
run name args =
  do  result <- liftIO $ runExceptT $ Utils.run name args
      either (throwError . Error.SystemCallFailed) return result


out :: String -> Manager.Manager ()
out string =
  let
    formattedString =
      if not (null string) && last string == '\n' then
        init string
      else
        string
  in
    liftIO $ hPutStrLn stdout formattedString

