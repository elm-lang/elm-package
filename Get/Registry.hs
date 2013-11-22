module Get.Registry where

import Control.Exception
import Control.Monad.Error
import Network.HTTP
import Network.Stream
import qualified Network.URI as URI
import Model.Dependencies
import qualified Model.Name as N
import Model.Version
import Data.Version (showVersion)
import qualified Paths_elm_get as This
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary

domain = "http://localhost:8000/"

metadata :: N.Name -> ErrorT String IO (Maybe Deps)
metadata name = do
    request "metadata" [("library", show name)] $ \body ->
        either throwError return $ Json.eitherDecode body

versions :: String -> ErrorT String IO (Maybe [Version])
versions library =
    request "versions" [("library", library)] (return . Binary.decode)

{--
register :: String -> String -> String -> ErrorT String IO String
register library version docs =
    request (postRequestWithBody url "text/json" docs) id
  where
    url = domain ++ "register?library=" ++ library ++ "&version=" ++ version
--}

request :: HStream ty => String -> [(String,String)] -> (ty -> ErrorT String IO a)
        -> ErrorT String IO a
request path vars handler =
    let request = domain ++ path ++ "?" ++ urlEncodeVars vars in
    case URI.parseURI request of
      Nothing  -> throwError $ "could not construct request: " ++ request
      Just uri -> do 
        result <- liftIO $ simpleHTTP (mkRequest GET uri) `catch`
                      \err -> let _ = err :: IOException
                              in return $ failMisc $ "Unable to connect to " ++ domain
        case result of
          Left (ErrorMisc str) -> throwError str
          Left err -> throwError (show err)
          Right response
              | rspCode response == (2,0,0) -> handler $ rspBody response
              | otherwise -> throwError $ "request to registry was unsuccessful"
