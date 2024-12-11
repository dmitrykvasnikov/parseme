{-# LANGUAGE StrictData #-}

module Error where

import           Data.List (intercalate)
import           Pos

data Message = Expected String
             | UnExpected String
             | Message String

instance Show Message where
  show (Expected msg)   = "Expected " <> msg
  show (UnExpected msg) = "Unexpected " <> msg
  show (Message msg)    = "Error " <> msg

instance Eq Message where
  Expected _ == Expected _     = True
  UnExpected _ == UnExpected _ = True
  Message _ == Message _       = True
  _ == _                       = False

data ParsemeError = ParsemeError { ePos :: SourcePos
                                 , msgs :: [Message]
                                 }

instance Show ParsemeError where
  show (ParsemeError pos msgs) =
    "Parsing error in "
      <> show pos
      <> "\n"
      <> intercalate "\n" (filtered . map show $ msgs)

filtered :: [String] -> [String]
filtered []       = []
filtered [x]      = [x]
filtered (x : xs) = if elem x xs then filtered xs else x : filtered xs

compareSoucePos :: SourcePos -> SourcePos -> Ordering
compareSoucePos l r = mappend (compare (sourceLine l) (sourceLine r)) (compare (sourceColumn l) (sourceColumn r))

-- merging errors based on consumed position (more consumed is preferrable)
mergeError :: ParsemeError -> ParsemeError -> ParsemeError
mergeError e1@(ParsemeError p1 m1) e2@(ParsemeError p2 m2)
  | null m1 && (not . null $ m2) = e2
  | null m2 && (not . null $ m1) = e1
  | otherwise = case compareSoucePos p1 p2 of
      EQ -> ParsemeError p1 (m1 <> m2)
      LT -> e2
      GT -> e1

newError :: SourcePos -> Message -> ParsemeError
newError pos msg = ParsemeError pos [msg]

-- errors for internal purposed (implementation of @pure@ etc ...)
unknownError :: SourcePos -> ParsemeError
unknownError pos = ParsemeError pos []

isUnknownError :: ParsemeError -> Bool
isUnknownError (ParsemeError _ msg) = null msg
