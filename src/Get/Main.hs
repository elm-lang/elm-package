{-# OPTIONS_GHC -W #-}
module Main where

import Control.Applicative
import Control.Monad.Error
import System.Directory (findExecutable)
import System.Exit
import System.IO

import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V

import qualified Get.Install as Install
import Get.Library
import Get.Options as Options
import qualified Get.Publish as Publish
import qualified Utils.Commands as Cmd
import qualified Get.Init as Init

main :: IO ()
main = do
  gitCheck
  cmd <- parse
  result <- runErrorT (handle cmd)
  case result of
    Right _ -> return ()
    Left err ->
        do hPutStr stderr ("\nError: " ++ err ++ newline)
           exitFailure
        where
          newline = if last err == '\n' then "" else "\n"

handle :: Command -> ErrorT String IO ()
handle options =
  case options of
    Install mLib ->
      case mLib of
        Nothing -> Install.installAll
        Just rawL -> do
          namedL <- updateName N.fromString' rawL
          vsndL  <- (updateVersion . updateMaybe) parseVsn namedL
          Install.install vsndL

    Publish -> Publish.publish

    Init -> lift Init.initialize

    Update _ -> Cmd.out "Not implemented yet!"
  where
    parseVsn s = case V.fromString s of
      Nothing -> throwError $ unlines $
            [ "tag " ++ s ++ " is not a valid version number."
            , "It must have the following format: 0.1.2 or 0.1.2-tag"
            ]
      Just s  -> return s

    updateMaybe :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
    updateMaybe up m =
        case m of
          Nothing -> pure Nothing
          Just x  -> Just <$> up x

errExit :: String -> IO ()
errExit msg = do
  hPutStrLn stderr msg
  exitFailure

gitCheck :: IO ()
gitCheck =
  do maybePath <- findExecutable "git"
     case maybePath of
       Just _  -> return ()
       Nothing -> errExit gitNotInstalledMessage
         
  where
    gitNotInstalledMessage =
        "\n\
        \The REPL relies on git to download libraries and manage versions.\n\
        \    It appears that you do not have git installed though!\n\
        \    Get it from <http://git-scm.com/downloads> to continue."
