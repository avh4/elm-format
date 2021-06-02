module Text.Parsec.Pos
  ( SourceName
  , SourcePos
  , newPos
  , sourceLine
  , sourceColumn
  ) where

import Parse.Primitives as EP


type SourceName = String


data SourcePos = SourcePos SourceName !EP.Row !EP.Col


newPos :: String -> EP.Row -> EP.Col -> SourcePos
newPos =
  SourcePos


sourceLine :: SourcePos -> Int
sourceLine (SourcePos _ row _) =
  fromIntegral row


sourceColumn :: SourcePos -> Int
sourceColumn (SourcePos _ _ col) =
  fromIntegral col


instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"
