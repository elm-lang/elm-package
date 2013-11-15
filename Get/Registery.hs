module Get.Registry where

import Control.Monad.Error
import Network.HTTP

url = "http://localhost:8000/"

install :: String -> ErrorT String IO String
install name = do
  result <- liftIO $ simpleHTTP (getRequest $ url ++ "install?library=" ++ name)
  case result of
    Left err -> throwError (show err)
    Right response
        | rspCode response == (2,0,0) -> return $ rspBody response
        | otherwise -> throwError $ rspBody response
