{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Http (send) where

import qualified Control.Exception as E
import Control.Monad.Error
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import Network (withSocketsDo)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types


send
    :: (MonadIO m, MonadError String m)
    => String
    -> (Request -> Manager -> IO a)
    -> m a
send url handler =
  do  result <- liftIO (sendSafe url handler)
      either throwError return result


sendSafe :: String -> (Request -> Manager -> IO a) -> IO (Either String a)
sendSafe url handler =
    sendUnsafe url handler
      `E.catch` handleHttpError url
      `E.catch` (handleAnyError url :: E.SomeException -> IO (Either String b))



sendUnsafe :: String -> (Request -> Manager -> IO a) -> IO (Either err a)
sendUnsafe url handler =
  do  request <- parseUrl url
      result <- withSocketsDo $ withManager tlsManagerSettings (handler request)
      return (Right result)


handleHttpError :: String -> HttpException -> IO (Either String b)
handleHttpError url exception =
  case exception of
    StatusCodeException (Status _code err) headers _ ->
        let details =
              case List.lookup "X-Response-Body-Start" headers of
                Just msg | not (BSC.null msg) -> msg
                _ -> err
        in
            return . Left $ BSC.unpack details

    _ -> handleAnyError url exception


handleAnyError :: (E.Exception e) => String -> e -> IO (Either String b)
handleAnyError url exception =
  return . Left $
      "failed with '" ++ show exception ++ "' when sending request to\n" ++
      "    <" ++ url ++ ">"
