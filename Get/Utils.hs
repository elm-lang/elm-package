module Get.Utils where         
         
import Control.Monad (forM_)
import System.Directory
import System.FilePath

inDir :: FilePath -> IO a -> IO a
inDir dir doStuff = do
  here <- getCurrentDirectory
  createDirectoryIfMissing True dir
  setCurrentDirectory dir
  result <- doStuff
  setCurrentDirectory here
  return result

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing True dst
  content <- getDirectoryContents src
  let paths = filter (`notElem` [".", "..",".git",".gitignore"]) content
  forM_ paths $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    (if isDirectory then copyDir else copyFile) srcPath dstPath

getUserAndProject :: String -> IO (String, String)
getUserAndProject lib =
    case length $ filter (=='/') lib of
      1 -> case break (=='/') lib of
             (user, project)
                 | user == "" || project == "/" -> failure
                 | otherwise                    -> return (user, tail project)
      _ -> failure

    where
      failure = error "project name is not well formed"
