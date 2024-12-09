{-# LANGUAGE FunctionalDependencies #-}

module Prim where

class Stream s t | s -> t where
  peek :: s -> Maybe (t, s)

instance Stream [Char] Char where
  peek []        = Nothing
  peek (c : str) = Just $ (c, str)
