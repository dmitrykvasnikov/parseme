{-# LANGUAGE StrictData #-}

module Pos where

type SourceName = String

type Line = Int

type Column = Int

data SourcePos = SourcePos SourceName Line Column
  deriving (Eq, Ord)

instance Show SourcePos where
  show (SourcePos name line column) = concat ["\"", name, "\"", " (line ", show line, ", column ", show column, ")"]

newPos :: SourceName -> Line -> Column -> SourcePos
newPos = SourcePos

initPos :: SourceName -> SourcePos
initPos name = newPos name 1 1

updateCharPos :: SourcePos -> Char -> SourcePos
updateCharPos (SourcePos name line column) ch
  | elem ch "\n\r" = SourcePos name (line + 1) 1
  | ch == '\t' = SourcePos name line (column + 8 - ((column - 1) `mod` 8))
  | otherwise = SourcePos name line (column + 1)

updateStringPos :: SourcePos -> String -> SourcePos
updateStringPos = foldl' updateCharPos

sourceLine :: SourcePos -> Line
sourceLine (SourcePos _ l _) = l

sourceColumn :: SourcePos -> Column
sourceColumn (SourcePos _ _ c) = c
