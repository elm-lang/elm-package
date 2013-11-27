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
  hFlush stdout
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

git :: [String] -> ErrorT String IO String
git = run "git"

run :: String -> [String] -> ErrorT String IO String
run command args =
  do result <- liftIO runCommand
     case result of
       Right out -> return out
       Left err -> throwError $
                   "failure when running:" ++ concatMap (' ':) (command:args) ++ "\n" ++ err
  where
    runCommand = do
      (_, Just out, Just err, handle) <-
          createProcess (proc command args) { std_out = CreatePipe
                                            , std_err = CreatePipe }
      exitCode <- waitForProcess handle
      result <- case exitCode of
                  ExitSuccess   -> readFrom out Right
                  ExitFailure _ -> readFrom err Left
      hClose out
      hClose err
      return result

    readFrom handle label = do
      msg <- hGetContents handle
      length msg `seq` return (label msg)

out :: String -> ErrorT String IO ()
out string = liftIO $ hPutStrLn stdout string'
    where
      string' = if not (null string) && last string == '\n' then init string else string

moduleToElmFile :: String -> FilePath
moduleToElmFile moduleName = swapDots moduleName ++ ".elm"

moduleToJsonFile :: String -> FilePath
moduleToJsonFile moduleName = "docs" </> swapDots moduleName ++ ".json"

swapDots :: String -> String
swapDots = map (\c -> if c == '.' then '/' else c)

combinedJson :: FilePath
combinedJson = "docs" </> "docs.json"