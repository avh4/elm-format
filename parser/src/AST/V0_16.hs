{-# OPTIONS_GHC -Wall #-}
module AST.V0_16 where

import qualified Reporting.Annotation as A

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
    deriving (Eq, Show)


type Comments = [Comment]


data Commented a =
    Commented Comments a Comments
    deriving (Eq, Show)


instance Functor Commented where
  fmap f (Commented pre a post) =
    Commented pre (f a) post


data KeywordCommented a =
  KeywordCommented Comments Comments a
  deriving (Eq, Show)


type PreCommented a = (Comments, a)


type PostCommented a = (a, Comments)


{-| This represents a list of things between clear start and end delimiters.
Comments can appear before and after any item, or alone if there are no items.

For example:
  ( {- nothing -} )
  ( a, b )
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
-}
data ExposedCommentedList a
    = Single a (Maybe String)
    | Multiple (PostCommented a) [Commented a] (PreCommented a) (Maybe String)


{-| This represents a list of things that have a clear start delimiter but no
clear end delimiter.
There must be at least one item.
Comments can appear before the last item, or around any other item.
An end-of-line comment can also appear after the last item.

For example:
  = a
  = a, b, c
-}
data OpenCommentedList a
    = OpenCommentedList [Commented a] (PreCommented a) (Maybe String)
    deriving (Eq, Show)

exposedToOpen :: Comments -> ExposedCommentedList a -> OpenCommentedList a
exposedToOpen pre exposed =
    case exposed of
        Single item eolComment ->
            OpenCommentedList [] (pre, item) eolComment

        Multiple (first', postFirst) rest' lst eolComment ->
            OpenCommentedList (Commented pre first' postFirst : rest') lst eolComment


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
    = IntNum Int IntRepresentation
    | FloatNum Double FloatRepresentation
    | Chr Char
    | Str String Bool
    | Boolean Bool
    deriving (Eq, Show)


data TypeConstructor
    = NamedConstructor [UppercaseIdentifier]
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show)


data Type'
    = UnitType Comments
    | TypeVariable LowercaseIdentifier
    | TypeConstruction TypeConstructor [(Comments, Type)]
    | TypeParens (Commented Type)
    | TupleType [Commented Type]
    | EmptyRecordType Comments
    | RecordType [(Commented LowercaseIdentifier, Commented Type, Bool)] Bool
    | RecordExtensionType (Commented LowercaseIdentifier) [(Commented LowercaseIdentifier, Commented Type, Bool)] Bool
    | FunctionType
        { first :: (Type, Maybe String)
        , rest :: [(Comments, Comments, Type, Maybe String)]
        , forceMultiline :: Bool
        }
    deriving (Eq, Show)


type Type =
    A.Located Type'
