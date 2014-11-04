{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Http where

import qualified Control.Exception as E
import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import Network (withSocketsDo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types


send
    :: (MonadIO m, MonadError String m)
    => String -> (Request -> Manager -> IO a) -> m a
send url handler =
    do result <- liftIO $ E.catch (Right `fmap` sendRequest) handleError
       either throwError return result
    where
      sendRequest = do
        request <- parseUrl url
        withSocketsDo $ withManager tlsManagerSettings (handler request)

      handleError exception =
          case exception of
            StatusCodeException (Status _code err) headers _ ->
                let details = case List.lookup "X-Response-Body-Start" headers of
                                Just msg | not (BSC.null msg) -> msg
                                _ -> err
                in  return . Left $ BSC.unpack details

            _ -> return . Left $
                 "failed with '" ++ show exception ++ "' when sending request to\n" ++
                 "    <" ++ url ++ ">"
