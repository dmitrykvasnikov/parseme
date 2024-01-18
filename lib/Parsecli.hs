module Parsecli where

import Parseme

ch = charP 'f'

parseCli :: IO()
parseCli = putStrLn "parsecli module"
