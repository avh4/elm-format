{-# LANGUAGE DeriveGeneric #-}
module ElmFormat.AST.Shared where

import Control.Applicative
import Data.Coapplicative
import Data.Int (Int64)
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Char as Char

{-| This module contains types that are used by multiple versions of the Elm AST.
-}


type List = []


newtype LowercaseIdentifier =
    LowercaseIdentifier String
    deriving (Eq, Ord)

instance Show LowercaseIdentifier where
    show (LowercaseIdentifier name) = name


newtype UppercaseIdentifier =
    UppercaseIdentifier String
    deriving (Eq, Ord, Show)


newtype SymbolIdentifier =
    SymbolIdentifier String
    deriving (Eq, Ord, Show)


data Commented c a =
    C c a
    deriving (Eq, Ord, Functor, Show) -- TODO: is Ord needed?

instance Monoid c => Applicative (Commented c) where
    pure = C mempty
    liftA2 f (C ca a) (C cb b) = C (ca <> cb) (f a b)

instance Foldable (Commented c) where
    foldMap f (C _ a) = f a

instance Traversable (Commented c) where
    sequenceA (C c1 fa) = C c1 <$> fa

instance Coapplicative (Commented c) where
    extract (C _ a) = a
    {-# INLINE extract #-}

instance Monoid c => Monad (Commented c) where
    (C c1 a) >>= f =
        C (c1 <> c2) b
        where (C c2 b) = f a


data IntRepresentation
  = DecimalInt
  | HexadecimalInt
  deriving (Eq, Show, Generic)


data FloatRepresentation
  = DecimalFloat
  | ExponentFloat
  deriving (Eq, Show, Generic)


data StringRepresentation
    = SingleQuotedString
    | TripleQuotedString
    deriving (Eq, Show, Generic)


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

refFromText :: Text -> Maybe (Ref ())
refFromText text =
    case Tuple.fst <$> Text.uncons text of
        Just first | Char.isUpper first ->
            Just $ TagRef () (UppercaseIdentifier $ Text.unpack text)

        Just first | Char.isLower first ->
            Just $ VarRef () (LowercaseIdentifier $ Text.unpack text)

        Just _ ->
            Just $ OpRef (SymbolIdentifier $ Text.unpack text)

        Nothing ->
            Nothing


data UnaryOperator =
    Negative
    deriving (Eq, Show)
