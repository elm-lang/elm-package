module Get.Dependencies where

import Elm.Internal.Dependencies
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V

defaultDeps :: Deps
defaultDeps = Deps
    { name = N.Name "USER" "PROJECT"
    , version = V.V [0,1] ""
    , summary = "concise, helpful summary of your project"
    , description = "full description of this project, describe your use case"
    , license = "BSD3"
    , repo = "https://github.com/USER/PROJECT.git"
    , exposed = []
    , elmVersion = V.elmVersion
    , dependencies = []
    }
