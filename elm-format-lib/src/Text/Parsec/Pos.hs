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
