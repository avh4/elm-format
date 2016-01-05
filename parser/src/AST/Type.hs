module AST.Type
    ( Type, Type'(..), TypeConstructor(..)
    , Port(..), getPortType
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
    = RUnit
    | RVar String
    | RApp TypeConstructor [Type]
    | RTuple [Type]
    | RRecord (Maybe String) [(String, Type, Bool)] Bool
    | RLambda Type [Type]
    deriving (Eq, Show)


data Port
    = Normal Type
    | Signal { root :: Type, arg :: Type }
    deriving (Show)


getPortType :: Port -> Type
getPortType portType =
  case portType of
    Normal tipe -> tipe
    Signal tipe _ -> tipe
