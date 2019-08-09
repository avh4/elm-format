{-# LANGUAGE DuplicateRecordFields #-}
module AST.V0_16 where

import qualified Reporting.Annotation as A
import Data.Int (Int64)


type List a = [a]


newtype ForceMultiline =
    ForceMultiline Bool
    deriving (Eq, Show)


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
    = BlockComment [String]
    | LineComment String
    | CommentTrickOpener
    | CommentTrickCloser
    | CommentTrickBlock String
    deriving (Eq, Ord, Show)


type Comments = [Comment]


data Commented a =
    Commented Comments a Comments
    deriving (Eq, Ord, Show)


instance Functor Commented where
  fmap f (Commented pre a post) =
    Commented pre (f a) post


data KeywordCommented a =
  KeywordCommented Comments Comments a
  deriving (Eq, Show)


type PreCommented a = (Comments, a)


type PostCommented a = (a, Comments)


data WithEol a = WithEol a (Maybe String)
    deriving (Eq, Show)


instance Functor WithEol where
    fmap f (WithEol a eol) = WithEol (f a) eol


{-| This represents a list of things separated by comments.

Currently, the first item will never have leading comments.
However, if Elm ever changes to allow optional leading delimiters, then
comments before the first delimiter will go there.
-}
type Sequence a =
    List ( Comments, PreCommented (WithEol a) )


{-| This represents a list of things between clear start and end delimiters.
Comments can appear before and after any item, or alone if there are no items.

For example:
  ( {- nothing -} )
  ( a, b )

TODO: this should be replaced with (Sequence a, Comments)
-}
data ContainedCommentedList a
    = Empty Comments
    | Items [Commented a]


{-| This represents a list of things that have no clear start and end
delimiters.

If there is more than one item in the list, then comments can appear.
Comments can appear after the first item, before the last item, and
around any other item.
An end-of-line comment can appear after the last item.

If there is only one item in the list, an end-of-line comment can appear after the item.

TODO: this should be replaced with (Sequence a)
-}
data ExposedCommentedList a
    = Single (WithEol a)
    | Multiple (PostCommented (WithEol a)) [Commented (WithEol a)] (PreCommented (WithEol a))


{-| This represents a list of things that have a clear start delimiter but no
clear end delimiter.
There must be at least one item.
Comments can appear before the last item, or around any other item.
An end-of-line comment can also appear after the last item.

For example:
  = a
  = a, b, c

TODO: this should be replaced with (Sequence a)
-}
data OpenCommentedList a
    = OpenCommentedList [Commented (WithEol a)] (PreCommented (WithEol a))
    deriving (Eq, Show)


instance Functor OpenCommentedList where
    fmap f (OpenCommentedList rest (pre, last)) =
        OpenCommentedList (fmap (fmap $ fmap f) rest) (pre, fmap f last)


exposedToOpen :: Comments -> ExposedCommentedList a -> OpenCommentedList a
exposedToOpen pre exposed =
    case exposed of
        Single item ->
            OpenCommentedList [] (pre, item)

        Multiple (first', postFirst) rest' lst ->
            OpenCommentedList (Commented pre first' postFirst : rest') lst


{-| Represents a delimiter-separated pair.

Comments can appear after the key or before the value.

For example:

  key = value
  key : value
-}
data Pair key value =
    Pair
        { _key :: PostCommented key
        , _value :: PreCommented value
        , forceMultiline :: ForceMultiline
        }
    deriving (Show, Eq)


instance Functor (Pair key) where
    fmap f (Pair key (pre, value) forceMultiline) =
        Pair key (pre, f value) forceMultiline


data Multiline
    = JoinAll
    | SplitAll
    deriving (Eq, Show)


isMultiline :: Multiline -> Bool
isMultiline JoinAll = False
isMultiline SplitAll = True


data FunctionApplicationMultiline
    = FASplitFirst
    | FAJoinFirst Multiline
    deriving (Eq, Show)


data IntRepresentation
  = DecimalInt
  | HexadecimalInt
  deriving (Eq, Show)


data FloatRepresentation
  = DecimalFloat
  | ExponentFloat
  deriving (Eq, Show)


data Literal
    = IntNum Int64 IntRepresentation
    | FloatNum Double FloatRepresentation
    | Chr Char
    | Str String Bool
    | Boolean Bool
    deriving (Eq, Show)


data TypeConstructor
    = NamedConstructor [UppercaseIdentifier] UppercaseIdentifier
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show)


data Type'
    = UnitType Comments
    | TypeVariable LowercaseIdentifier
    | TypeConstruction TypeConstructor [(Comments, Type)]
    | TypeParens (Commented Type)
    | TupleType [Commented (WithEol Type)]
    | RecordType
        { base :: Maybe (Commented LowercaseIdentifier)
        , fields :: Sequence (Pair LowercaseIdentifier Type)
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | FunctionType
        { first :: WithEol Type
        , rest :: [(Comments, Comments, Type, Maybe String)]
        , forceMultiline :: ForceMultiline
        }
    deriving (Eq, Show)


type Type =
    A.Located Type'
