{-# OPTIONS_GHC -Wall #-}
module AST.V0_16 where

import qualified Reporting.Annotation as A


data Comment
    = BlockComment [String]
    | LineComment String
    deriving (Eq, Show)


data Commented a =
    Commented [Comment] a [Comment]
    deriving (Eq, Show)


data Literal
    = IntNum Int
    | FloatNum Double
    | Chr Char
    | Str String Bool
    | Boolean Bool
    deriving (Eq, Ord, Show)


data TypeConstructor
    = NamedConstructor String
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show)


data Type'
    = UnitType [Comment]
    | TypeVariable String
    | TypeConstruction TypeConstructor [([Comment], Type)]
    | TypeParens (Commented Type)
    | TupleType [Commented Type]
    | EmptyRecordType [Comment]
    | RecordType [(Commented String, Commented Type, Bool)] Bool
    | RecordExtensionType (Commented String) [(Commented String, Commented Type, Bool)] Bool
    | FunctionType Type [Type]
    deriving (Eq, Show)


type Type =
    A.Located Type'
