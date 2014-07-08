module Get.Init where

import System.IO (hFlush, stdout)
import Control.Applicative ((<$>))

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Dependencies as D
import qualified Data.ByteString.Lazy as BS

askForChecked :: (String -> Either String a) -> String -> IO a
askForChecked check request = do
  putStr $ request ++ " "
  hFlush stdout
  answer <- getLine
  case check answer of
    Right result -> return result
    Left message -> do putStrLn message
                       askForChecked check request

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe def val = case val of
  Just r -> Right r
  Nothing -> Left def

askForVersion :: String -> IO V.Version
askForVersion req = askForChecked check req
  where check = (eitherFromMaybe "Wrong version format!" . V.fromString)

askFor :: String -> IO String
askFor req = askForChecked Right req

askForLimited :: String -> Int -> String -> IO String
askForLimited name limit req = askForChecked check req
  where check str = if length str > limit
                    then Left (concat [name, " length shouldn't exceed ", show limit, " characters!"])
                    else Right str

readDeps :: IO D.Deps
readDeps = do
  projectName <- askFor "Project name:"
  userName <- askFor "Github user name:"
  version <- askForVersion "Initial version?"
  summary <- askForLimited "Summary" 80 "Summary:"
  description <- askFor "Description:"
  license <- askFor "License?"
  repo <- askFor "Repository address?"
  elmVersion <- askForVersion "Elm version?"
  return $ D.Deps (N.Name userName projectName) version summary description license repo [] [] elmVersion []

initialize :: IO ()
initialize = do
  dependencies <- readDeps
  BS.writeFile "elm_dependencies.json" (encodePretty dependencies)
