{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Happstack.Server hiding (body,port)
import Happstack.Server.Compression
import Happstack.Server.FileServe.BuildingBlocks (serveDirectory')
import qualified Happstack.Server as Happs

import Control.Monad.Error
import Control.Exception
import System.Console.CmdArgs
import System.FilePath as FP
import System.Process
import System.Directory
import GHC.Conc

import Data.Acid (closeAcidState)
import qualified DB.LibraryVersions as DB
import System.Environment

data Flags = Flags
  { port :: Int
  } deriving (Data,Typeable,Show,Eq)

flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  }

{-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  cargs <- cmdArgs flags
  putStrLn $ "Serving at <localhost:" ++ show (port cargs) ++ ">"
  simpleHTTP nullConf { Happs.port = port cargs } $ do
    compressedResponseFilter
    decodeBody $ defaultBodyPolicy "/tmp/" 0 10000 1000
    msum [ dir "register" (ok $ toResponse emptyIDE)
         , dir "install" (ok $ toResponse emptyIDE)
         ]
--}
------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          acid <- DB.open
          case args of
            [key]
              -> do either <- runErrorT $ DB.versions acid key
                    case either of
                      Left err -> putStrLn err
                      Right [] -> putStrLn $ key ++ " has no associated value."
                      Right vs -> putStrLn $ key ++ " = " ++ show vs
            [key,val]
              -> do print args
                    either <- runErrorT $ DB.register acid key val
                    case either of
                      Left err -> putStrLn err
                      Right () -> putStrLn "Done."
            _ -> do putStrLn "Usage:"
                    putStrLn "  key               Lookup the value of 'key'."
                    putStrLn "  key value         Set the value of 'key' to 'value'."
          closeAcidState acid