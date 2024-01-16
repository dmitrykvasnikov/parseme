module Parseme where

import Data.Char
import Control.Monad
import Control.Applicative

data Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = Parser $ \inp -> let [(o, rest)] = runParser p inp
                              in [(f o, rest)]

instance Applicative Parser where
  pure v    = Parser $ \inp -> [(v, inp)]
  pf <*> pv = Parser $ \inp -> let [(f, rest)] = runParser pf inp
                                   [(o, rest')] = runParser pv rest
                               in [(f o, rest')]

instance Alternative Parser where
  empty = Parser $ \inp -> []
  p1 <|> p2 = Parser $ \inp ->
              case runParser p1 inp of
                []  -> runParser p2 inp
                res -> res

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \inp -> let [(o, rest)] = runParser p inp
                             in runParser (f o) rest

instance MonadPlus Parser where
mzero = Parser $ \inp -> []
mplus p1 p2 = Parser $ \inp -> runParser p1 inp <> runParser p2 inp

zeroP :: Parser String
zeroP = Parser $ \int -> []

parseme :: IO ()
parseme = putStrLn "parseme"

-- to avoice collisiont between Control.Monad and Parseme
plusP = Parseme.mplus

-- Simpler parsers
item :: Parser Char
item = Parser $ \inp ->
  case inp of
    ""     -> []
    (c:cs) -> [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \inp ->
  case inp of
    (c:cs) -> if p c then [(c,cs)] else []
    []     -> []

charP :: Char -> Parser Char
charP c = sat (== c)

digitP :: Parser Char
digitP = sat isDigit

lowerP :: Parser Char
lowerP = sat isLower

upperP :: Parser Char
upperP = sat isUpper

stringP :: String -> Parser String
stringP [] = return ""
stringP s  = mapM charP s

