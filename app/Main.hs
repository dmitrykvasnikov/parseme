module Main where

import Control.Monad
import Control.Applicative
import Parseme
import Parsecli


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  parseme
  parseCli
