{-# LANGUAGE StrictData #-}

module Pos where

type Column = Int

type Line = Int

data Pos = Pos Column Line
  deriving (Eq, Ord)
