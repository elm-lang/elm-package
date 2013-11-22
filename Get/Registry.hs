module Get.Registry where

import Control.Exception
import Control.Monad.Error
import Network.HTTP
import Network.Stream
import Network.URI (parseURI)
import Model.Dependencies
import Model.Name
import Model.Version
import Data.Version (showVersion)
import qualified Paths_elm_get as This
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary

domain = "http://localhost:8000/"

latest :: Name -> ErrorT String IO (Maybe Deps)
latest library =
    request (domain ++ "latest?library=" ++ show library) $ \body ->
        either throwError return $ Json.eitherDecode body

versions :: String -> ErrorT String IO (Maybe [Version])
versions library =
    request (domain ++ "versions?library=" ++ library) (return . Binary.decode)

{--
register :: String -> String -> String -> ErrorT String IO String
register library version docs =
    request (postRequestWithBody url "text/json" docs) id
  where
    url = domain ++ "register?library=" ++ library ++ "&version=" ++ version
--}

request :: HStream ty => String -> (ty -> ErrorT String IO a) -> ErrorT String IO a
request request handler =
    case parseURI $ request ++ "&version=" ++ showVersion This.version of
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
