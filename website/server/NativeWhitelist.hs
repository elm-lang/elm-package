{-# LANGUAGE OverloadedStrings #-}
module NativeWhitelist (verify) where

import Control.Monad.Error
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Internal.Dependencies as D

whitelist :: FilePath
whitelist = "whitelist.json"

load :: IO [String]
load =
  do exists <- Dir.doesFileExist whitelist
     case exists of
       False -> return []
       True ->
           withBinaryFile whitelist ReadMode $ \handle -> do
             json <- LBS.hGetContents handle
             LBS.length json `seq` return (maybe [] id (Json.decode json))

verify :: D.Deps -> ErrorT String IO ()
verify deps =
    case D.native deps of
      [] -> return ()
      _  -> do
        names <- liftIO load
        if show (D.name deps) `elem` names
           then return ()
           else throwError whitelistError

whitelistError :: String
whitelistError =
    "You are trying to publish a project that has native-modules. For now,\n\
    \    any modules that use Native code must go through a formal review process\n\
    \    to make sure the exposed API is pure and the Native code is absolutely\n\
    \    necessary. Please open an issue with the title \"Native review for ____\"\n\
    \    to begin the review process: <https://github.com/elm-lang/elm-get/issues>"
