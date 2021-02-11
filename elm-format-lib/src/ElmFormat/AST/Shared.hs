module ElmFormat.AST.Shared where

{-| This module contains types that are used by multiple versions of the Elm AST.
-}

import qualified Data.Maybe as Maybe
import Data.Coapplicative
import Data.Int (Int64)


type List a = [a]


data LowercaseIdentifier =
    LowercaseIdentifier String
    deriving (Eq, Ord, Show)


data UppercaseIdentifier =
    UppercaseIdentifier String
    deriving (Eq, Ord, Show)


data SymbolIdentifier =
    SymbolIdentifier String
    deriving (Eq, Ord, Show)


data Comment
    = BlockComment (List String)
    | LineComment String
    | CommentTrickOpener
    | CommentTrickCloser
    | CommentTrickBlock String
    deriving (Eq, Ord, Show)
type Comments = List Comment

eolToComment :: Maybe String -> Comments
eolToComment eol =
    Maybe.maybeToList (fmap LineComment eol)


data Commented c a =
    C c a
    deriving (Eq, Ord, Functor, Show) -- TODO: is Ord needed?

instance Coapplicative (Commented c) where
    extract (C _ a) = a
    {-# INLINE extract #-}

type C1 l1 a = Commented Comments a
type C2 l1 l2 a = Commented (Comments, Comments) a
type C3 l1 l2 l3 a = Commented (Comments, Comments, Comments) a

type C0Eol a = Commented (Maybe String) a
type C1Eol l1 a = Commented (Comments, Maybe String) a
type C2Eol l1 l2 a = Commented (Comments, Comments, Maybe String) a


data IntRepresentation
  = DecimalInt
  | HexadecimalInt
  deriving (Eq, Show)


data FloatRepresentation
  = DecimalFloat
  | ExponentFloat
  deriving (Eq, Show)


data StringRepresentation
    = SingleQuotedString
    | TripleQuotedString
    deriving (Eq, Show)


data LiteralValue
    = IntNum Int64 IntRepresentation
    | FloatNum Double FloatRepresentation
    | Chr Char
    | Str String StringRepresentation
    | Boolean Bool
    deriving (Eq, Show)


data Ref ns
    = VarRef ns LowercaseIdentifier
    | TagRef ns UppercaseIdentifier
    | OpRef SymbolIdentifier
    deriving (Eq, Ord, Show, Functor)


data UnaryOperator =
    Negative
    deriving (Eq, Show)
