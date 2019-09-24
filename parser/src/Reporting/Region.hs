{-# LANGUAGE OverloadedStrings #-}
module Reporting.Region where

import qualified Data.String as String
import qualified Text.Parsec.Pos as Parsec


data Region = Region
    { start :: Position
    , end :: Position
    }
    deriving (Eq)


instance Show Region where
    showsPrec p r = showParen (p > 10) $
        showString $ String.unwords
            [ "at"
            , show (line $ start r)
            , show (column $ start r)
            , show (line $ end r)
            , show (column $ end r) 
            ]


data Position = Position
    { line :: Int
    , column :: Int
    }
    deriving (Eq, Show)


fromSourcePos :: Parsec.SourcePos -> Position
fromSourcePos sourcePos =
    Position
      (Parsec.sourceLine sourcePos)
      (Parsec.sourceColumn sourcePos)


merge :: Region -> Region -> Region
merge (Region start _) (Region _ end) =
    Region start end


-- TO STRING

toString :: Region -> String
toString (Region start end) =
  case line start == line end of
    False ->
        "between lines " ++ show (line start)
        ++ " and " ++ show (line end)

    True ->
        "on line " ++ show (line end) ++ ", column "
        ++ show (column start) ++ " to " ++ show (column end)
