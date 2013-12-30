module Main where

import System.Directory
import System.Environment
import System.Exit
import System.IO
import Registry.Generate.Listing

-- Run this program with:
--   runhaskell -isrc src/Registry/Remover.hs [PROJECT]

main = do
  args <- getArgs
  case args of
    [name] -> do setCurrentDirectory "website"
                 remove name
                 putStrLn "remove function called successfully"
    _      -> do hPutStrLn stderr "Bad command line arguments. Expecting a single project name."
                 exitFailure