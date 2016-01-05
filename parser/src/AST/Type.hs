module AST.Type
    ( Type, Type'(..), TypeConstructor(..)
    ) where

import qualified Reporting.Annotation as A


-- DEFINITION

type Type =
    A.Located Type'


data TypeConstructor
    = NamedConstructor String
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show)


data Type'
    = UnitType
    | TypeVariable String
    | TypeConstruction TypeConstructor [Type]
    | TupleType [Type]
    | RecordType (Maybe String) [(String, Type, Bool)] Bool
    | FunctionType Type [Type]
    deriving (Eq, Show)
