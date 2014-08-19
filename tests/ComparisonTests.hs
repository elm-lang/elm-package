{-# LANGUAGE OverloadedStrings #-}

module ComparisonTests (comparisonTests) where

import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text (Text)
import Control.Monad.Error (runErrorT)
import Test.HUnit ((@?), (@?=), Assertion)
import qualified Data.Map as Map
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TH

import qualified Utils.SemverCheck as SC

-- SETUP

type_const1 = function [var "a", var "b"] (var "a")
type_const2 = function [var "b", var "a"] (var "b")

type_int = adt "Int" []
type_string = adt "String" []

const_renaming = Map.fromList [("a", "b"), ("b", "a")]

yieldsRenaming :: Value -> Value -> Map String String -> Assertion
yieldsRenaming v1 v2 renaming =
  case parseEither (runErrorT . SC.buildRenaming Map.empty) (v1, v2) of
    Left err -> False @? "Parsing JSON values should succeed"
    Right (Left _) -> False @? "Non-compatibility shouldn't occur"
    Right (Right result) -> result @?= renaming

nonCompatible :: Value -> Value -> Assertion
nonCompatible v1 v2 =
  case parseEither (runErrorT . SC.buildRenaming Map.empty) (v1, v2) of
    Left err -> False @? "Parsing JSON values should succeed"
    Right (Left _) -> return ()
    Right (Right _) -> False @? "Types shouldn't be compatible"


-- SETUP HELPERS

var :: Text -> Value
var varName =
  object [ "tag" .= ("var" :: Text)
         , "name" .= varName
         ]

function :: [Value] -> Value -> Value
function args result =
  object [ "tag" .= ("function" :: Text)
         , "args" .= args
         , "result" .= result
         ]

adt :: Text -> [Value] -> Value
adt name args =
  object [ "tag" .= ("adt" :: Text)
         , "name" .= name
         , "args" .= args
         ]


comparisonTests =
  TF.testGroup "Types comparison tests"
  [ TH.testCase "a -> b -> a == b -> a -> b" (yieldsRenaming type_const1 type_const2 const_renaming)
  , TH.testCase "Int /= String" (nonCompatible type_int type_string)
  , TH.testCase "String /= Int" (nonCompatible type_string type_int)
  ]
