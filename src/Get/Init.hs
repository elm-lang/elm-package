module Get.Init where

import System.IO (hFlush, stdout)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Package.Name as N
import qualified Package.Version as V
import qualified Package.Description as D
import qualified Package.Paths as Path
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

injectDefault :: Maybe [a] -> [a] -> [a]
injectDefault (Just xs) [] = xs
injectDefault _ ys = ys

askForVersion :: Maybe String -> String -> IO V.Version
askForVersion def req = askForChecked check req
  where check = (eitherFromMaybe "Wrong version format!" . V.fromString . injectDefault def)

askFor :: String -> IO String
askFor req = askForChecked Right req

askForWithDefault :: String -> String -> IO String
askForWithDefault def req = askForChecked (Right . injectDefault (Just def)) req

askForLimited :: String -> Int -> String -> IO String
askForLimited name limit req = askForChecked check req
  where check str = if length str > limit
                    then Left errorMessage
                    else Right str
        errorMessage = concat [ name
                              , " length shouldn't exceed "
                              , show limit
                              , " characters!"]

readDeps :: IO D.Description
readDeps = do
  projectName <- askFor "Project name:"
  userName <- askFor "Github user name:"
  version <- askForVersion (Just "0.1.0") "Initial version? [default: 0.1.0]"
  summary <- askForLimited "Summary" 80 "Summary:"
  description <- askFor "Description:"
  license <- askForWithDefault "BSD3" "License? [default: BSD3]"
  repo <- askFor "Repository address?"
  elmVersion <- askForVersion Nothing "Elm version?"
  return $ D.Description (N.Name userName projectName) version summary description license repo [] [] elmVersion [] []

initialize :: IO ()
initialize = do
  dependencies <- readDeps
  BS.writeFile Path.description (encodePretty dependencies)
