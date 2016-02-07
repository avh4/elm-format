{-# OPTIONS_GHC -Wall #-}
module AST.V0_16 where

import qualified Reporting.Annotation as A


data Comment
    = BlockComment [String]
    | LineComment String
    deriving (Eq, Show)


type Comments = [Comment]


data Commented a =
    Commented Comments a Comments
    deriving (Eq, Show)


instance Functor Commented where
  fmap f (Commented pre a post) =
    Commented pre (f a) post


type PreCommented a = (Comments, a)


type PostCommented a = (a, Comments)


data IntRepresentation
  = DecimalInt
  | HexadecimalInt
  deriving (Eq, Show)


data Literal
    = IntNum Int IntRepresentation
    | FloatNum Double
    | Chr Char
    | Str String Bool
    | Boolean Bool
    deriving (Eq, Show)


data TypeConstructor
    = NamedConstructor String
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show)


data Type'
    = UnitType Comments
    | TypeVariable String
    | TypeConstruction TypeConstructor [(Comments, Type)]
    | TypeParens (Commented Type)
    | TupleType [Commented Type]
    | EmptyRecordType Comments
    | RecordType [(Commented String, Commented Type, Bool)] Bool
    | RecordExtensionType (Commented String) [(Commented String, Commented Type, Bool)] Bool
    | FunctionType (Type, Comments) [Commented Type] (Comments, Type)
    deriving (Eq, Show)


type Type =
    A.Located Type'
