module Text.Parsec.Pos
  ( SourceName
  , SourcePos
  , sourceLine
  , sourceColumn
  ) where

import Parse.Primitives as EP


type SourceName = String


data SourcePos = SourcePos SourceName !EP.Row !EP.Col


sourceLine :: SourcePos -> Int
sourceLine = undefined


sourceColumn :: SourcePos -> Int
sourceColumn = undefined


instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"
