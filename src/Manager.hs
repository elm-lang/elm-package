module Manager where

import Control.Monad.Reader
import Control.Monad.Error


type Manager a =
    ErrorT String IO a


data Environment = Environment
    { catalog :: String
    }


data Command
    = Install (Maybe (String, Maybe String))
    | Publish


run :: Manager a -> IO (Either String a)
run manager =
    runErrorT manager
