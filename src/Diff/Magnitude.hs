{-# OPTIONS_GHC -Wall #-}
module Diff.Magnitude
  ( Magnitude(..)
  )
  where


data Magnitude
    = PATCH
    | MINOR
    | MAJOR
    deriving (Eq, Ord, Show)

