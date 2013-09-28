{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit hiding (modes)
import Data.Version (showVersion)
--import qualified Paths_elm_get as Path
import System.Exit

data Options = Options
    { library :: String
    , hats :: Int
    } deriving (Data,Typeable,Show,Eq)

options = Options
  { library = def &= args &= typ "LIBRARY"
  , hats = 0
         &= help "How many hats?"
  }

data Command = Help | Install | Update | Publish
    deriving (Data, Typeable, Show, Eq)

routes =
    modes [Help &= auto, Install, Update, Publish]
    &= program "elm-get"
    &= help "Install, update, and publish Elm libraries."
--    &= summary ("elm-get " ++ showVersion Path.version ++ ", (c) Evan Czaplicki")

main = do
  print =<< cmdArgs routes
