{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Registry.Server where

import Happstack.Server hiding (body,port)
import Happstack.Server.Compression
import qualified Happstack.Server as Happs
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Maybe as Maybe

import Control.Applicative
import Control.Monad.Error
import Control.Exception
import System.Console.CmdArgs
import System.FilePath as FP
import System.Process
import System.Directory
import GHC.Conc

import System.Environment
import qualified Registry.Utils as Utils
import qualified Model.Name as N
import qualified Model.Version as V

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
  createDirectoryIfMissing True Utils.libDir
  putStrLn $ "Serving at <localhost:" ++ show (port cargs) ++ ">"
  simpleHTTP nullConf { Happs.port = port cargs } $ do
    compressedResponseFilter
    decodeBody $ defaultBodyPolicy "/tmp/" 10000 1000 1000
    msum [ dir "register" register
         , dir "versions" versions
         , dir "metadata" metadata
         , badRequest $ toResponse $ ("did not work\n" :: String)
         ]

type SPResponse = ServerPart Response

register :: SPResponse
register =
  do method POST
     with args format $ \(directory, tempDocs) -> do
       exists <- liftIO $ doesDirectoryExist directory
       case exists of
         True ->
             badRequest $ toResponse ("That version has already been registered." :: String)
         False -> do
           let permanentDocs = directory </> Utils.json
           liftIO $ do createDirectory directory
                       BS.writeFile permanentDocs =<< BS.readFile tempDocs
                       buildDocs permanentDocs
           ok $ toResponse ("Registered successfully!" :: String)
  where
    args = (,,) <$> look "library" <*> look "version" <*> lookFile "docs"

    format (name', version', (docsPath,_,_)) = do
      name <- N.fromString name'
      version <- V.fromString version'
      return (Utils.libraryVersion name version, docsPath)

buildDocs :: FilePath -> IO ()
buildDocs path = return ()

versions :: ServerPart Response
versions =
  do method POST
     with (look "library") N.fromString $ \name -> do
       let path = Utils.library name
       exists <- liftIO $ doesDirectoryExist path
       versions <- case exists of
                     False -> return Nothing
                     True  -> do contents <- liftIO $ getDirectoryContents path
                                 return $ Just $ Maybe.mapMaybe V.fromString contents
       ok $ toResponseBS (C.pack "text/plain") (Binary.encode versions)

metadata :: ServerPart Response
metadata =
  do method POST
     with (look "library") N.fromString $ \name -> do
       let path = Utils.library name
       exists <- liftIO $ doesDirectoryExist path
       case exists of
         False -> notFound $ toResponse ("There is no library " ++ show name)
         True  -> serveJson path name
  where
    serveJson path name = do
      contents <- liftIO $ getDirectoryContents path
      case Maybe.mapMaybe V.fromString contents of
        [] -> notFound $ toResponse ("No registered versions of " ++ show name)
        versions -> do
          either <- getVersion name versions `fmap` getDataFn (look "version")
          case either of
            Left err -> notFound $ toResponse err
            Right version ->
                do contents <- liftIO $ getDirectoryContents (Utils.libraryVersion name version)
                   serveFile (asContentType "application/json")
                             (Utils.libraryVersion name version </> Utils.json)

    getVersion name versions either =
      case either of
        Left _ -> Right $ maximum versions
        Right str ->
            case V.fromString str of
              Nothing -> Left $ "Requested an invalid version number: " ++ str
              Just v | v `elem` versions -> Right v
                     | otherwise ->
                         Left $ "Library " ++ show name ++ " has no version " ++ str

with :: RqData a -> (a -> Maybe b) -> (b -> SPResponse) -> SPResponse
with args format handle = do
  either <- getDataFn args
  case either of
    Left err -> notFound $ toResponse $ unlines err
    Right info ->
        case format info of
          Just value -> handle value
          Nothing -> let msg = "The request arguments are not well-formed."
                     in  notFound $ toResponse (msg :: String)
