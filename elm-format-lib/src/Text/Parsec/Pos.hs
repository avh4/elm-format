module Text.Parsec.Pos
  ( SourceName
  , SourcePos
  , sourceLine
  , sourceColumn
  ) where

type SourceName = String

type Line = Int

type Column = Int

data SourcePos = SourcePos SourceName !Line !Column

sourceLine :: SourcePos -> Line
sourceLine = undefined

sourceColumn :: SourcePos -> Column
sourceColumn = undefined


instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"
