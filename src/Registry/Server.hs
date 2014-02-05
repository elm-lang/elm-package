{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import           Control.Applicative
import           Control.Monad.Error
import qualified Data.ByteString        as BS
import qualified Data.HashMap.Strict    as Map
import           GHC.Conc
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.Console.CmdArgs
import           System.Directory
import           System.Exit
import           System.IO

import qualified Elm.Internal.Name      as N
import qualified Elm.Internal.Paths     as Elm
import qualified Elm.Internal.Version   as V
import qualified Registry.Generate.Docs as Docs
import qualified Registry.Generate.Html as Html
import qualified Registry.Routes        as Route
import qualified Utils.Paths            as Path

data Flags = Flags
  { port       :: Int
  , regenerate :: Bool
  } deriving (Data,Typeable,Show,Eq)

flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  , regenerate = False &= help "flag to regenerate all documentation"
  }

-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  getRuntimeAndDocs
  setupLogging
  setupSrcHtml
  createDirectoryIfMissing True Path.libDir
  cargs <- cmdArgs flags
  when (regenerate cargs) Docs.regenerate
  httpServe (setPort (port cargs) defaultConfig) $
      ifTop (serveFile "public/Home.html")
      <|> route [ ("catalog"  , Route.catalog)
                , ("versions" , Route.versions)
                , ("register" , Route.register)
                , ("metadata" , Route.metadata)
                , ("resources", serveDirectoryWith directoryConfig "resources")
                ]
      <|> serveDirectoryWith directoryConfig "public"
      <|> do modifyResponse $ setResponseStatus 404 "Not found"
             serveFile "public/Error404.html"

getRuntimeAndDocs :: IO ()
getRuntimeAndDocs = do
  BS.writeFile "resources/elm-runtime.js" =<< BS.readFile Elm.runtime
  BS.writeFile "resources/docs.json" =<< BS.readFile Elm.docs

setupLogging :: IO ()
setupLogging =
    do createDirectoryIfMissing True "log"
       createIfMissing "log/access.log"
       createIfMissing "log/error.log"
    where
      createIfMissing path = do
        exists <- doesFileExist path
        when (not exists) $ BS.writeFile path ""

setupSrcHtml :: IO ()
setupSrcHtml =
    do createDirectoryIfMissing True "public"
       result <- runErrorT generate
       case result of
         Right _ -> return ()
         Left err -> do hPutStrLn stderr err
                        exitFailure
    where
      generate = mapM Html.generateSrc $ map (\name -> "src/" ++ name ++ ".elm") elms
      elms = ["Error404","Catalog","Home","DesignGuidelines","Documentation"]

directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexGenerator = defaultIndexGenerator defaultMimeTypes indexStyle
    , mimeTypes = Map.insert ".elm" "text/plain" $
                  Map.insert ".ico" "image/x-icon" $ defaultMimeTypes
    }

indexStyle :: BS.ByteString
indexStyle =
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \div.footer { display:none; }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"
