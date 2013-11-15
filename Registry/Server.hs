{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Registry.Server where

import Happstack.Server hiding (body,port)
import Happstack.Server.Compression
import qualified Happstack.Server as Happs

import Control.Applicative
import Control.Monad.Error
import Control.Exception
import System.Console.CmdArgs
import System.FilePath as FP
import System.Process
import System.Directory
import GHC.Conc

import Data.Acid (closeAcidState)
import qualified Registry.DB.LibraryVersions as DB
import System.Environment

data Flags = Flags
  { port :: Int
  } deriving (Data,Typeable,Show,Eq)

flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  }

-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  cargs <- cmdArgs flags
  acid <- DB.open
  putStrLn $ "Serving at <localhost:" ++ show (port cargs) ++ ">"
  simpleHTTP nullConf { Happs.port = port cargs } $ do
    compressedResponseFilter
    decodeBody $ defaultBodyPolicy "/tmp/" 0 1000 1000
    msum [ dir "register" (register acid)
         , dir "versions" (versions acid)
         , dir "latest" (latest acid)
         , badRequest $ toResponse $ ("did not work\n" :: String)
         ]

register :: DB.LibVer -> ServerPart Response
register acid =
  do method GET
     request acid args (uncurry (DB.register acid)) handle
  where
    args = (,) <$> look "library" <*> look "version"
    handle either =
        case either of
          Left err -> badRequest $ toResponse err
          Right () -> ok $ toResponse $ ("it worked!" :: String)

versions :: DB.LibVer -> ServerPart Response
versions acid =
  do method GET
     request acid (look "library") (DB.versions acid) $ \either ->
         case either of
           Left err -> notFound $ toResponse err
           Right vs -> ok $ toResponse $ (show vs :: String)

latest :: DB.LibVer -> ServerPart Response
latest acid = do
  method GET
  request acid (look "library") (DB.latestUntagged acid) $ \either ->
    case either of
      Left err -> notFound $ toResponse err
      Right version -> ok $ toResponse $ version

request :: DB.LibVer
        -> RqData a
        -> (a -> ErrorT e IO a')
        -> (Either e a' -> ServerPart Response)
        -> ServerPart Response
request acid info transaction handle = do
  either <- getDataFn info
  case either of
    Left err -> badRequest $ toResponse $ unlines err
    Right value ->
        do either <- liftIO $ runErrorT $ transaction value
           handle either
