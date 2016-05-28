{-# LANGUAGE OverloadedStrings #-}
module Utils.Http (send) where

import qualified Control.Exception as E
import Control.Monad.Except (throwError, liftIO)
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http

import qualified Reporting.Error as Error
import qualified Manager



-- PUBLIC API


send :: String -> (Http.Request -> Http.Manager -> IO a) -> Manager.Manager a
send url handler =
  do  manager <- Reader.asks Manager.httpManager
      result <- liftIO (sendSafe url manager handler)
      either throwError return result



-- ACTUALLY SEND REQUESTS


sendSafe
  :: String
  -> Http.Manager
  -> (Http.Request -> Http.Manager -> IO a)
  -> IO (Either Error.Error a)
sendSafe url manager handler =
  sendUnsafe url manager handler
    `E.catch` handleHttpError url
    `E.catch` \e -> handleAnyError url (e :: E.SomeException)


sendUnsafe
  :: String
  -> Http.Manager
  -> (Http.Request -> Http.Manager -> IO a)
  -> IO (Either err a)
sendUnsafe url manager handler =
  do  request <- Http.parseUrl url
      result <- handler request manager
      return (Right result)


handleHttpError :: String -> Http.HttpException -> IO (Either Error.Error b)
handleHttpError url exception =
  case exception of
    Http.StatusCodeException (Http.Status _code err) headers _ ->
      return $ Left $ Error.HttpRequestFailed url $ BSC.unpack $
        case List.lookup "X-Response-Body-Start" headers of
          Just msg | not (BSC.null msg) ->
            msg

          _ ->
            err

    _ ->
      handleAnyError url exception


handleAnyError :: (E.Exception e) => String -> e -> IO (Either Error.Error b)
handleAnyError url exception =
  return $ Left $ Error.HttpRequestFailed url (show exception)
