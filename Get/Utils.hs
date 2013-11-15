module Get.Utils where         
         
import Control.Monad (forM_)
import Control.Monad.Error
import System.Directory
import System.FilePath

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
      (user, '/' : project) | okay user && okay library -> return (user, project)
      _ -> throwError $ "Project names must be formatted like this: user/project"
    where
      okay str = not (null str) && length (filter (=='/') str) /= 1
