module Registry.Remover where

import System.Environment
import System.Exit
import System.IO
import Registry.Generate.Listing

main = do
  args <- getArgs
  case args of
    [name] -> remove name >> putStrLn "remove function called successfully"
    _ -> do hPutStrLn stderr "Bad command line arguments. Expecting project name."
            exitFailure