module Get.Init where

import System.IO (hFlush, stdout)
import Control.Applicative ((<$>))

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Dependencies as D
import qualified Data.ByteString.Lazy as BS

askForChecked :: (String -> Maybe a) -> String -> String -> IO a
askForChecked check request errorMsg = do
  putStr $ request ++ " "
  hFlush stdout
  answer <- getLine
  case check answer of
    Just result -> return result
    Nothing -> do putStrLn errorMsg
                  askForChecked check request errorMsg

askForVersion req = askForChecked V.fromString req "Wrong version format!" 
askFor req = askForChecked Just req undefined

readDeps :: IO D.Deps
readDeps = do
  projectName <- askFor "Project name:"
  userName <- askFor "Github user name:"
  version <- askForVersion "Initial version?"
  summary <- askFor "Summary:"
  description <- askFor "Description:"
  license <- askFor "License?"
  repo <- askFor "Repository address?"
  elmVersion <- askForVersion "Elm version?"
  return $ D.Deps (N.Name userName projectName) version summary description license repo [] [] elmVersion []

initialize :: IO ()
initialize = do
  dependencies <- readDeps
  BS.writeFile "elm_dependencies.json" (encodePretty dependencies)
