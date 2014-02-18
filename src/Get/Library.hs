module Get.Library where

import Control.Applicative

import Elm.Internal.Name as N
import Elm.Internal.Version as V

type RawLibrary = Library' String (Maybe String)
type Library    = Library' N.Name (Maybe String)
type VsnLibrary = Library' N.Name V.Version
data Library' name version
  = Library'
    { lib     :: name
    , version :: version
    }
    deriving (Show, Eq)

updateName :: (Functor f) => (n1 -> f n2) -> Library' n1 v -> f (Library' n2 v)
updateName upper l = case l of
  Library' { lib = n } ->
    (\name ->
      l { lib = name }
    )
    <$>
    upper n

updateVersion :: (Functor f) => (v1 -> f v2) -> Library' n v1 -> f (Library' n v2)
updateVersion upper l = case l of
  Library' { version = v } ->
    (\vsn ->
      l { version = vsn }
    )
    <$>
    upper v
