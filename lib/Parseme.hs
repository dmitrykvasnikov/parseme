{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Parseme where

import Data.Char
import Control.Monad
import Control.Applicative

-- data structure for input string
data Input = Input { inputLoc :: Int
                   , inputStr :: String
                   } deriving Show

-- helper for consumption a char from input strem, returning tuple of new position and rest of the input stream
item :: Input -> Maybe (Char, Input)
item (Input _ []) = Nothing
item (Input loc (c:cs)) = Just (c, Input (loc + 1) cs)

-- helper to run parser for testing
run :: Parseme a -> String -> Either ParseError (a, Input)
run p i = parser p (Input 0 i)

-- data structure for error
data ParseError = ParseError Int String

instance Show ParseError where
  show (ParseError loc err) = "ERROR:\npos#: " <> (show loc) <> "\n" <> err

-- data structure for parser
data Parseme a = Parseme { parser :: Input -> Either ParseError (a, Input) }

instance Functor Parseme where
 fmap f pf = Parseme $ \inp ->
  do
    (v, rest) <- parser pf inp
    return (f v, rest)

instance Applicative Parseme where
  pure v    = Parseme $ \inp -> Right (v, inp)
  pf <*> pv = Parseme $ \inp ->
    do
      (f, rest')  <- parser pf inp
      (v, rest'') <- parser pv rest'
      return (f v, rest'')

instance Alternative (Either ParseError) where
  empty         = Left $ ParseError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _      = e1

instance Alternative Parseme where
  empty     = Parseme $ const empty
  --  p1 <|> p2 = Parseme $ \inp -> parser p1 inp <|> parser p2 inp
  p1 <|> p2 = Parseme $ \inp ->
    case parser p1 inp of
      Right v -> Right v
      Left _  -> parser p2 inp

instance Monad Parseme where
  return   = pure
  mp >>= f = Parseme $ \inp ->
     case parser mp inp of
        Right (o, rest) -> parser (f o) rest
        Left err        -> Left err

-- parser helpers

str :: Char -> String
str x = [x]

trim :: Parseme a -> Parseme a
trim p  = ws *> p <* ws

-- check for End of Input stream (isSpace not working in this case)
eos :: Parseme String
eos = Parseme $ \inp ->
  case inputStr inp of
    (x:_) -> Left $ ParseError (inputLoc inp) ("Unexpected symbol: \'" ++ [x] ++ "\', probaly space is missed")
    []     -> Right ("", inp)

charP :: Char -> Parseme Char
charP c = Parseme f where
  f input@(item -> Just (x, xs))
    | c == x    = Right (x, xs)
    | otherwise = Left $ ParseError (inputLoc input) ("Can't match char: expected " <> [c] <> " with actual " <> [x])
  f input = Left $ ParseError (inputLoc input) "Unexpected end of input string"

wordP :: String -> Parseme String
wordP w = Parseme $ \inp ->
  case parser (traverse charP w) inp of
    Left _ -> Left $ ParseError
                       (inputLoc inp)
                       ("Can't match string: expected \"" <> w
                       <> "\" with actual \"" <> take (length w) (inputStr inp) <> "\"")
    result -> result

parseIf :: String           -- predicate description
        -> (Char -> Bool)   -- predicate
        -> Parseme Char
parseIf desc pr = Parseme $ \inp ->
  case inp of
    (item -> Just (x, xs))
      | pr x      -> Right (x, xs)
      | otherwise -> Left $ ParseError (inputLoc inp) ("The symbol '" <> [x] <> "' doen't satisfy the condition: " <> desc)
    _             -> Left $ ParseError (inputLoc inp) ("Unexpected end of unput string parsing the condition: " <> desc)

-- separartor: sepBy element separator
sepP :: Parseme a -> Parseme b -> Parseme [a]
sepP el sep = (:) <$> el <*> many (sep *> el)


digitP, letterP, upperP, lowerP, spaceP, alnumP, dotP :: Parseme Char
digitP  = parseIf "digit" isDigit
letterP = parseIf "letter" isLetter
upperP  = parseIf "upper letters" isUpper
lowerP  = parseIf "lower letters" isLower
spaceP  = parseIf "space" isSpace
alnumP  = digitP <|> letterP
dotP    = charP '.'

someP :: Parseme a -> Parseme [a]
someP p  = some'
  where
    many' = some' <|> pure []
    some' = (:) <$> p <*> many'

manyP :: Parseme a -> Parseme [a]
manyP p  = many'
  where
    many' = some' <|> pure []
    some' = (:) <$> p <*> many'

maybeP :: Parseme String -> Parseme String
maybeP p = Parseme $ \inp ->
  case parser p inp of
    Right res -> Right res
    Left _  -> Right ("", inp)

-- skip whitespaces
ws :: Parseme String
ws      = manyP spaceP

ws1 :: Parseme String
ws1     = someP spaceP

-- skip till
skipP :: (Char -> Bool) -> Parseme String
skipP p = (manyP . parseIf "skipping") $ not . p


-- number Integer
numIntP :: Parseme Integer
numIntP = Parseme $ \inp ->
  case parser (some digitP) inp of
    Right (n, rest) -> Right (read n, rest)
    Left err        -> Left err

-- number Double
numDoubleP :: Parseme Double
numDoubleP = Parseme $ \inp ->
  case parser ((,) <$> someP digitP <*> maybeP (dotP *> someP digitP) <* (ws1 <|> eos)) inp of
    Right ((a,b), rest) -> Right ((read (a ++ "." ++ b)) :: Double, rest)
    Left err              -> Left err

-- string
stringP :: Parseme String
stringP = charP '\"' *> skipP (=='\"') <* charP '\"'


test = [wordP "--file-name", str <$> charP '=']

parseme :: IO ()
parseme = putStrLn "parseme"
