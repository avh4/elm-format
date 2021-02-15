{-# LANGUAGE TypeFamilies #-}
module ElmFormat.AST.Shared where

{-| This module contains types that are used by multiple versions of the Elm AST.
-}

import qualified Data.Maybe as Maybe
import Data.Coapplicative
import Data.Int (Int64)


type List a = [a]


data LowercaseIdentifier =
    LowercaseIdentifier String
    deriving (Eq, Ord)

instance Show LowercaseIdentifier where
    show (LowercaseIdentifier name) = name


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

type C1 l1 = Commented Comments
type C2 l1 l2 = Commented (Comments, Comments)
type C3 l1 l2 l3 = Commented (Comments, Comments, Comments)

type C0Eol = Commented (Maybe String)
type C1Eol l1 = Commented (Comments, Maybe String)
type C2Eol l1 l2 = Commented (Comments, Comments, Maybe String)

class ToCommentedList f where
    type CommentsFor f :: * -> *
    toCommentedList :: f a -> List (CommentsFor f a)


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
