{-# LANGUAGE OverloadedStrings #-}
module Utils.Http where

import Network
import Network.HTTP.Types
import Network.HTTP.Conduit

import Control.Monad.Error
import Control.Monad.Trans.Resource
import qualified Control.Exception as E

import qualified Data.List as List
import qualified Data.ByteString.Char8 as BSC

send :: (Manager -> ResourceT IO a) -> ErrorT String IO a
send request =
    do result <- liftIO $ E.catch (Right `fmap` mkRequest) handler
       either throwError return result
    where
      mkRequest = withSocketsDo $ withManager request

      handler exception =
          case exception of
            sce@(StatusCodeException (Status code err) headers _) ->
                let details = case List.lookup "X-Response-Body-Start" headers of
                                Just msg | not (BSC.null msg) -> msg
                                _ -> err
                in  return . Left $ BSC.unpack details

            _ -> return . Left $
                 "probably unable to connect to <http://library.elm-lang.org> (" ++
                 show exception ++ ")"