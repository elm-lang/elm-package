module Get.Registry where

import Control.Exception
import Control.Monad.Error
import Network.HTTP
import Network.Stream
import Network.URI (parseURI)
import Model.Version
import qualified Data.Binary as Binary

domain = "http://localhost:8000/"

latest :: String -> ErrorT String IO String
latest library =
    request (getRequest $ domain ++ "latest?library=" ++ library) id

versions :: String -> ErrorT String IO (Maybe [Version])
versions library =
    let parse = parseURI $ domain ++ "versions?library=" ++ library in
    case parse of
      Nothing -> throwError "Could not request versions for that library."
      Just uri -> request (mkRequest GET uri) Binary.decode

register :: String -> String -> String -> ErrorT String IO String
register library version docs =
    request (postRequestWithBody url "text/json" docs) id
  where
    url = domain ++ "register?library=" ++ library ++ "&version=" ++ version

request :: HStream ty => Request ty -> (ty -> a) -> ErrorT String IO a
request request handler = do
  result <- liftIO $ simpleHTTP request `catch` \err ->
                          let _ = err :: IOException
                          in  return $ failMisc $ "Unable to connect to " ++ domain
  case result of
    Right response
        | rspCode response == (2,0,0) -> return $ handler $ rspBody response
        | otherwise -> throwError $ "Error"

    Left (ErrorMisc str) -> throwError str
    Left err -> throwError (show err)
