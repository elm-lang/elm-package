module Main where

import qualified Test.Framework as TF

import SolverTests

main = TF.defaultMain [ solverTests ]
