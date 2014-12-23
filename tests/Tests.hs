module Main where

import qualified Test.Framework as TF

--import SolverTests
import ComparisonTests

main = TF.defaultMain [ comparisonTests ]
