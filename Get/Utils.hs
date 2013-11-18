module Get.Utils where         
         
import Control.Monad (forM_)
import Control.Monad.Error
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

root = "elm_dependencies"
internals = "_internals"
depsFile = "elm_dependencies.json"

yesOrNo :: IO Bool
yesOrNo = do
  input <- getLine
  case input of
    "y" -> return True
    "n" -> return False
    _   -> do putStr "Must type 'y' for yes or 'n' for no: "
              yesOrNo

inDir :: FilePath -> ErrorT String IO a -> ErrorT String IO a
inDir dir doStuff = do
  here <- liftIO $ getCurrentDirectory
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ setCurrentDirectory dir
  result <- doStuff
  liftIO $ setCurrentDirectory here
  return result

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = liftIO $ do
  createDirectoryIfMissing True dst
  content <- getDirectoryContents src
  let paths = filter (`notElem` [".", "..",".git",".gitignore"]) content
  forM_ paths $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    (if isDirectory then copyDir else copyFile) srcPath dstPath

getUserAndProject :: String -> ErrorT String IO (String, String)
getUserAndProject library =
    case break (=='/') library of
      (user, '/' : project) | okay user && okay project -> return (user, project)
      _ -> throwError $ "project names must be formatted like this: user/project"
    where
      okay str = not (null str) && length (filter (=='/') str) /= 1

git :: [String] -> ErrorT String IO String
git args =
  do (exitCode, output) <- liftIO runCommand
     case exitCode of
       ExitSuccess -> return output
       ExitFailure _ -> throwError $ "failure when running: git" ++ concatMap (' ':) args
  where
    runCommand = do
      (_, Just out, Just err, handle) <-
          createProcess (proc "git" args) { std_out = CreatePipe
                                          , std_err = CreatePipe }
      exitCode <- waitForProcess handle
      str <- hGetContents out
      hClose out
      hClose err
      return (exitCode, str)

out :: String -> ErrorT String IO ()
out string = liftIO $ hPutStrLn stdout string'
    where
      string' = if not (null string) && last string == '\n' then init string else string
