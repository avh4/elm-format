{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.V0_16 (module AST.V0_16, module ElmFormat.AST.Shared) where

import Data.Bifunctor
import Data.Coapplicative
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Compose
import qualified Data.Indexed as I
import Data.Kind
import qualified Cheapskate.Types as Markdown
import ElmFormat.AST.Shared
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Bifoldable (Bifoldable(..))


newtype ForceMultiline =
    ForceMultiline Bool
    deriving (Eq, Show)

instance Semigroup ForceMultiline where
    (ForceMultiline a) <> (ForceMultiline b) = ForceMultiline (a || b)


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


data CommentType
    = BeforeTerm
    | AfterTerm
    | Inside
    | BeforeSeparator
    | AfterSeparator

type C1 (l1 :: CommentType) = Commented Comments
type C2 (l1 :: CommentType) (l2 :: CommentType) = Commented (Comments, Comments)
type C3 (l1 :: CommentType) (l2 :: CommentType) (l3 :: CommentType) = Commented (Comments, Comments, Comments)

type C0Eol = Commented (Maybe String)
type C1Eol (l1 :: CommentType) = Commented (Comments, Maybe String)
type C2Eol (l1 :: CommentType) (l2 :: CommentType) = Commented (Comments, Comments, Maybe String)

class AsCommentedList f where
    type CommentsFor f :: Type -> Type
    toCommentedList :: f a -> List (CommentsFor f a)
    fromCommentedList :: List (CommentsFor f a) -> Either Text (f a)


type CommentedMap k v =
    Map k (C2 'BeforeTerm 'AfterTerm v)

mergeCommentedMap :: Ord k => (v -> v -> v) -> CommentedMap k v -> CommentedMap k v -> CommentedMap k v
mergeCommentedMap merge left right =
    let
        merge' (C (pre1, post1) a) (C (pre2, post2) b) =
            C (pre1 ++ pre2, post1 ++ post2) (merge a b)
    in
    Map.unionWith merge' left right


{-| This represents a list of things separated by comments.

Currently, the first item will never have leading comments.
However, if Elm ever changes to allow optional leading delimiters, then
comments before the first delimiter will go there.
-}
newtype Sequence a =
    Sequence (List (C2Eol 'BeforeSeparator 'AfterSeparator a))
    deriving (Eq, Functor, Show)

instance Foldable Sequence where
    foldMap f (Sequence items) = foldMap (f . extract) items

instance Semigroup (Sequence a) where
    (Sequence left) <> (Sequence right) = Sequence (left <> right)

instance Monoid (Sequence a) where
    mempty = Sequence []

instance AsCommentedList Sequence where
    type CommentsFor Sequence = C2Eol 'BeforeSeparator 'AfterSeparator
    toCommentedList (Sequence items) = items
    fromCommentedList = Right . Sequence


{-| This represents a list of things between clear start and end delimiters.
Comments can appear before and after any item, or alone if there are no items.

For example:
  ( {- nothing -} )
  ( a, b )

TODO: this should be replaced with (Sequence a, Comments)
-}
data ContainedCommentedList a
    = Empty (C1 'Inside ())
    | Items [C2 'BeforeTerm 'AfterTerm a]


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
    = Single (C0Eol a)
    | Multiple (C1Eol 'AfterTerm a) [C2Eol 'BeforeTerm 'AfterTerm a] (C1Eol 'BeforeTerm a)


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
    = OpenCommentedList [C2Eol 'BeforeTerm 'AfterTerm a] (C1Eol 'BeforeTerm a)
    deriving (Eq, Show, Functor, Foldable)

instance AsCommentedList OpenCommentedList where
    type CommentsFor OpenCommentedList = C2Eol 'BeforeTerm 'AfterTerm
    toCommentedList (OpenCommentedList rest (C (cLast, eolLast) last)) =
        rest ++ [ C (cLast, [], eolLast) last ]
    fromCommentedList list =
        case reverse list of
            C (cLast, cLastInvalid, eolLast) last : revRest ->
                Right $ OpenCommentedList
                    (reverse revRest)
                    (C (cLast ++ cLastInvalid, eolLast) last)

            [] ->
                Left "AsCommentedList may not be empty"


exposedToOpen :: Comments -> ExposedCommentedList a -> OpenCommentedList a
exposedToOpen pre exposed =
    case exposed of
        Single (C eol item) ->
            OpenCommentedList [] (C (pre, eol) item)

        Multiple (C (postFirst, eol) first') rest' lst ->
            OpenCommentedList (C (pre, postFirst, eol) first' : rest') lst


{-| Represents a delimiter-separated pair.

Comments can appear after the key or before the value.

For example:

  key = value
  key : value
-}
data Pair key value =
    Pair
        { _key :: C1 'AfterTerm key
        , _value :: C1 'BeforeTerm value
        , forceMultiline :: ForceMultiline
        }
    deriving (Show, Eq, Functor, Foldable)

instance Bifunctor Pair where
    bimap fa fb (Pair a b fm) =
        Pair (fa <$> a) (fb <$> b) fm


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


data Assoc = L | N | R
    deriving (Eq, Show)

assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


data NameWithArgs name arg =
    NameWithArgs name [C1 'BeforeTerm arg]
    deriving (Eq, Show, Functor, Foldable)

instance Bifunctor NameWithArgs where
    bimap fname farg (NameWithArgs name args) =
        NameWithArgs (fname name) (fmap farg <$> args)


data SourceTag
  = Normal
  | Effect Comments
  | Port Comments
  deriving (Eq, Show)


type SourceSettings =
    [ ( C2 'BeforeTerm 'AfterTerm LowercaseIdentifier
      , C2 'BeforeTerm 'AfterTerm UppercaseIdentifier
      )
    ]


-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a
    = ExplicitListing a Bool
    | OpenListing (C2 'BeforeTerm 'AfterTerm ())
    | ClosedListing
    deriving (Eq, Ord, Show, Functor) -- TODO: is Ord needed?

mergeListing :: (a -> a -> a) -> Listing a -> Listing a -> Listing a
mergeListing merge left right =
    case (left, right) of
        (ClosedListing, ClosedListing) -> ClosedListing
        (ClosedListing, OpenListing comments) -> OpenListing comments
        (OpenListing comments, ClosedListing) -> OpenListing comments
        (OpenListing (C (pre1, post1) ()), OpenListing (C (pre2, post2) ())) -> OpenListing (C (pre1 ++ pre2, post1 ++ post2) ())
        (ClosedListing, ExplicitListing a multiline) -> ExplicitListing a multiline
        (ExplicitListing a multiline, ClosedListing) -> ExplicitListing a multiline
        (OpenListing comments, ExplicitListing _a _multiline) -> OpenListing comments
        (ExplicitListing _a _multiline, OpenListing comments) -> OpenListing comments
        (ExplicitListing a multiline1, ExplicitListing b multiline2) -> ExplicitListing (merge a b) (multiline1 || multiline2)


-- | A value that can be imported or exported
data ListingValue
    = Value !LowercaseIdentifier
    | OpValue SymbolIdentifier
    | Union (C1 'AfterTerm UppercaseIdentifier) (Listing (CommentedMap UppercaseIdentifier ()))
    deriving (Eq, Ord, Show) -- TODO: is Ord needed?


data DetailedListing = DetailedListing
    { values :: CommentedMap LowercaseIdentifier ()
    , operators :: CommentedMap SymbolIdentifier ()
    , types :: CommentedMap UppercaseIdentifier (C1 'BeforeTerm (Listing (CommentedMap UppercaseIdentifier ())))
    }
    deriving (Eq, Show)

instance Semigroup DetailedListing where
    (DetailedListing av ao at) <> (DetailedListing bv bo bt) = DetailedListing (av <> bv) (ao <> bo) (at <> bt)

instance Monoid DetailedListing where
    mempty = DetailedListing mempty mempty mempty


data TypeConstructor ctorRef
    = NamedConstructor ctorRef
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show, Functor, Foldable)


data BinopsClause varRef expr =
    BinopsClause Comments varRef Comments expr
    deriving (Eq, Show, Functor)

instance Bifunctor BinopsClause where
    bimap fvr fe = \case
        BinopsClause c1 vr c2 e -> BinopsClause c1 (fvr vr) c2 (fe e)

instance Bifoldable BinopsClause where
    bifoldMap fa fb (BinopsClause _ a _ b) = fa a <> fb b


data IfClause e =
    IfClause (C2 'BeforeTerm 'AfterTerm e) (C2 'BeforeTerm 'AfterTerm e)
    deriving (Eq, Show, Functor, Foldable)


data TopLevelStructure a
    = DocComment Markdown.Blocks
    | BodyComment Comment
    | Entry a
    deriving (Eq, Show, Functor, Foldable)


data LocalName
    = TypeName UppercaseIdentifier
    | CtorName UppercaseIdentifier
    | VarName LowercaseIdentifier
    deriving (Eq, Ord, Show)


data NodeKind
    = ModuleNK
    | ImportMethodNK
    | ModuleListingNK
    | ModuleHeaderNK
    | ModuleBodyNK
    | TypeRefNK
    | CtorRefNK
    | VarRefNK
    | CommonDeclarationNK
    | TopLevelDeclarationNK
    | ExpressionNK
    | LetDeclarationNK
    | CaseBranchNK
    | PatternNK
    | TypeNK


class ASTParameters p where
    type TypeRef p :: Type
    type CtorRef p :: Type
    type VarRef p :: Type

data VariableNamespace (ns :: Type)
instance ASTParameters (VariableNamespace ns) where
    type TypeRef (VariableNamespace ns) = (ns, UppercaseIdentifier)
    type CtorRef (VariableNamespace ns) = (ns, UppercaseIdentifier)
    type VarRef (VariableNamespace ns) = Ref ns


data AST p (getType :: NodeKind -> Type) (kind :: NodeKind) where

    --
    -- Singletons
    --

    TypeRef_ :: TypeRef p -> AST p getType 'TypeRefNK
    CtorRef_ :: CtorRef p -> AST p getType 'CtorRefNK
    VarRef_ :: VarRef p -> AST p getType 'VarRefNK

    --
    -- Module
    --

    Module ::
        { initialComments :: Comments
        , header :: Maybe (getType 'ModuleHeaderNK)
        , docs :: Maybe Markdown.Blocks
        , imports :: C1 'BeforeTerm (Map [UppercaseIdentifier] (C1 'BeforeTerm (getType 'ImportMethodNK)))
        , moduleBody :: getType 'ModuleBodyNK
        }
        -> AST p getType 'ModuleNK
    ImportMethod ::
        { alias :: Maybe (C2 'BeforeSeparator 'AfterSeparator UppercaseIdentifier)
        , exposedVars :: C2 'BeforeSeparator 'AfterSeparator (getType 'ModuleListingNK)
        }
        -> AST p getType 'ImportMethodNK
    ModuleListing ::
        Listing DetailedListing
        -> AST p getType 'ModuleListingNK
    ModuleHeader ::
        { srcTag :: SourceTag
        , name :: C2 'BeforeTerm 'AfterTerm [UppercaseIdentifier]
        , moduleSettings :: Maybe (C2 'BeforeSeparator 'AfterSeparator SourceSettings)
        , exports :: Maybe (C2 'BeforeSeparator 'AfterSeparator (getType 'ModuleListingNK))
        }
        -> AST p getType 'ModuleHeaderNK
    ModuleBody ::
        [TopLevelStructure (getType 'TopLevelDeclarationNK)]
        -> AST p getType 'ModuleBodyNK

    --
    -- Declarations
    --

    Definition ::
        getType 'PatternNK
        -> [C1 'BeforeTerm (getType 'PatternNK)]
        -> Comments
        -> getType 'ExpressionNK
        -> AST p getType 'CommonDeclarationNK
    TypeAnnotation ::
        C1 'AfterTerm (Ref ())
        -> C1 'BeforeTerm (getType 'TypeNK)
        -> AST p getType 'CommonDeclarationNK
    CommonDeclaration ::
        getType 'CommonDeclarationNK
        -> AST p getType 'TopLevelDeclarationNK
    Datatype ::
        { nameWithArgs :: C2 'BeforeTerm 'AfterTerm (NameWithArgs UppercaseIdentifier LowercaseIdentifier)
        , tags :: OpenCommentedList (NameWithArgs UppercaseIdentifier (getType 'TypeNK))
        }
        -> AST p getType 'TopLevelDeclarationNK
    TypeAlias ::
        Comments
        -> C2 'BeforeTerm 'AfterTerm (NameWithArgs UppercaseIdentifier LowercaseIdentifier)
        -> C1 'BeforeTerm (getType 'TypeNK)
        -> AST p getType 'TopLevelDeclarationNK
    PortAnnotation ::
        C2 'BeforeTerm 'AfterTerm LowercaseIdentifier
        -> Comments
        -> getType 'TypeNK
        -> AST p getType 'TopLevelDeclarationNK
    PortDefinition_until_0_16 ::
        C2 'BeforeTerm 'AfterTerm LowercaseIdentifier
        -> Comments
        -> getType 'ExpressionNK
        -> AST p getType 'TopLevelDeclarationNK
    Fixity_until_0_18 ::
        Assoc
        -> Comments
        -> Int
        -> Comments
        -> getType 'VarRefNK
        -> AST p getType 'TopLevelDeclarationNK
    Fixity ::
        C1 'BeforeTerm Assoc
        -> C1 'BeforeTerm Int
        -> C2 'BeforeTerm 'AfterTerm SymbolIdentifier
        -> C1 'BeforeTerm LowercaseIdentifier
        -> AST p getType 'TopLevelDeclarationNK

    --
    -- Expressions
    --

    Unit :: Comments -> AST p getType 'ExpressionNK
    Literal :: LiteralValue -> AST p getType 'ExpressionNK
    VarExpr :: getType 'VarRefNK -> AST p getType 'ExpressionNK

    App ::
        getType 'ExpressionNK
        -> List (C1 'BeforeTerm (getType 'ExpressionNK))
        -> FunctionApplicationMultiline
        -> AST p getType 'ExpressionNK
    Unary ::
        UnaryOperator
        -> getType 'ExpressionNK
        -> AST p getType 'ExpressionNK
    Binops ::
        getType 'ExpressionNK
        -> List (BinopsClause (getType 'VarRefNK) (getType 'ExpressionNK)) -- Non-empty
        -> Bool
        -> AST p getType 'ExpressionNK
    Parens ::
        C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)
        -> AST p getType 'ExpressionNK

    ExplicitList ::
        { terms :: Sequence (getType 'ExpressionNK)
        , trailingComments_el :: Comments
        , forceMultiline_el :: ForceMultiline
        }
        -> AST p getType 'ExpressionNK
    Range ::
        C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)
        -> C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)
        -> AST p getType 'ExpressionNK

    Tuple ::
        [C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)]
        -> Bool
        -> AST p getType 'ExpressionNK
    TupleFunction ::
        Int -- will be 2 or greater, indicating the number of elements in the tuple
        -> AST p getType 'ExpressionNK

    Record ::
        { base_r :: Maybe (C2 'BeforeTerm 'AfterTerm LowercaseIdentifier)
        , fields_r :: Sequence (Pair LowercaseIdentifier (getType 'ExpressionNK))
        , trailingComments_r :: Comments
        , forceMultiline_r :: ForceMultiline
        }
        -> AST p getType 'ExpressionNK
    Access ::
        getType 'ExpressionNK
        -> LowercaseIdentifier
        -> AST p getType 'ExpressionNK
    AccessFunction ::
        LowercaseIdentifier
        -> AST p getType 'ExpressionNK

    Lambda ::
        [C1 'BeforeTerm (getType 'PatternNK)]
        -> Comments
        -> getType 'ExpressionNK
        -> Bool
        -> AST p getType 'ExpressionNK
    If ::
        IfClause (getType 'ExpressionNK)
        -> [C1 'BeforeTerm (IfClause (getType 'ExpressionNK))]
        -> C1 'BeforeTerm (getType 'ExpressionNK)
        -> AST p getType 'ExpressionNK
    Let ::
        [getType 'LetDeclarationNK]
        -> Comments
        -> getType 'ExpressionNK
        -> AST p getType 'ExpressionNK
    LetCommonDeclaration ::
        getType 'CommonDeclarationNK
        -> AST p getType 'LetDeclarationNK
    LetComment ::
        Comment
        -> AST p getType 'LetDeclarationNK
    Case ::
        (C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK), Bool)
        -> [getType 'CaseBranchNK]
        -> AST p getType 'ExpressionNK
    CaseBranch ::
        { beforePattern :: Comments
        , beforeArrow :: Comments
        , afterArrow :: Comments
        , pattern :: getType 'PatternNK
        , caseBranchBody :: getType 'ExpressionNK
        }
        -> AST p getType 'CaseBranchNK

    -- for type checking and code gen only
    GLShader ::
        String
        -> AST p getType 'ExpressionNK


    --
    -- Patterns
    --

    Anything ::
        AST p getType 'PatternNK
    UnitPattern ::
        Comments
        -> AST p getType 'PatternNK
    LiteralPattern ::
        LiteralValue
        -> AST p getType 'PatternNK
    VarPattern ::
        LowercaseIdentifier
        -> AST p getType 'PatternNK
    OpPattern ::
        SymbolIdentifier
        -> AST p getType 'PatternNK
    DataPattern ::
        getType 'CtorRefNK
        -> [C1 'BeforeTerm (getType 'PatternNK)]
        -> AST p getType 'PatternNK
    PatternParens ::
        C2 'BeforeTerm 'AfterTerm (getType 'PatternNK)
        -> AST p getType 'PatternNK
    TuplePattern ::
        [C2 'BeforeTerm 'AfterTerm (getType 'PatternNK)]
        -> AST p getType 'PatternNK
    EmptyListPattern ::
        Comments
        -> AST p getType 'PatternNK
    ListPattern ::
        [C2 'BeforeTerm 'AfterTerm (getType 'PatternNK)]
        -> AST p getType 'PatternNK
    ConsPattern ::
        { first_cp :: C0Eol (getType 'PatternNK)
        , rest_cp :: Sequence (getType 'PatternNK)
        }
        -> AST p getType 'PatternNK
    EmptyRecordPattern ::
        Comments
        -> AST p getType 'PatternNK
    RecordPattern ::
        [C2 'BeforeTerm 'AfterTerm LowercaseIdentifier]
        -> AST p getType 'PatternNK
    Alias ::
        C1 'AfterTerm (getType 'PatternNK)
        -> C1 'BeforeTerm LowercaseIdentifier
        -> AST p getType 'PatternNK


    --
    -- Types
    --

    UnitType ::
        Comments
        -> AST p getType 'TypeNK
    TypeVariable ::
        LowercaseIdentifier
        -> AST p getType 'TypeNK
    TypeConstruction ::
        TypeConstructor (getType 'TypeRefNK)
        -> [C1 'BeforeTerm (getType 'TypeNK)]
        -> ForceMultiline
        -> AST p getType 'TypeNK
    TypeParens ::
        C2 'BeforeTerm 'AfterTerm (getType 'TypeNK)
        -> AST p getType 'TypeNK
    TupleType ::
        NonEmpty (C2Eol 'BeforeTerm 'AfterTerm (getType 'TypeNK))
        -> ForceMultiline
        -> AST p getType 'TypeNK
    RecordType ::
        { base_rt :: Maybe (C2 'BeforeTerm 'AfterTerm LowercaseIdentifier)
        , fields_rt :: Sequence (Pair LowercaseIdentifier (getType 'TypeNK))
        , trailingComments_rt :: Comments
        , forceMultiline_rt :: ForceMultiline
        }
        -> AST p getType 'TypeNK
    FunctionType ::
        { first_ft :: C0Eol (getType 'TypeNK)
        , rest_ft :: Sequence (getType 'TypeNK)
        , forceMultiline_ft :: ForceMultiline
        }
        -> AST p getType 'TypeNK

deriving instance
    ( Eq (getType 'ModuleNK)
    , Eq (getType 'ImportMethodNK)
    , Eq (getType 'ModuleListingNK)
    , Eq (getType 'ModuleHeaderNK)
    , Eq (getType 'ModuleBodyNK)
    , Eq (getType 'CommonDeclarationNK)
    , Eq (getType 'TopLevelDeclarationNK)
    , Eq (getType 'TypeRefNK)
    , Eq (getType 'CtorRefNK)
    , Eq (getType 'VarRefNK)
    , Eq (getType 'ExpressionNK)
    , Eq (getType 'LetDeclarationNK)
    , Eq (getType 'CaseBranchNK)
    , Eq (getType 'PatternNK)
    , Eq (getType 'TypeNK)
    , Eq (TypeRef p) -- TODO: is there a way to not need UndecidableInstances for this?
    , Eq (CtorRef p)
    , Eq (VarRef p)
    ) =>
    Eq (AST p getType kind)
deriving instance
    ( Show (getType 'ModuleNK)
    , Show (getType 'ImportMethodNK)
    , Show (getType 'ModuleListingNK)
    , Show (getType 'ModuleHeaderNK)
    , Show (getType 'ModuleBodyNK)
    , Show (getType 'CommonDeclarationNK)
    , Show (getType 'TopLevelDeclarationNK)
    , Show (getType 'TypeRefNK)
    , Show (getType 'CtorRefNK)
    , Show (getType 'VarRefNK)
    , Show (getType 'ExpressionNK)
    , Show (getType 'LetDeclarationNK)
    , Show (getType 'CaseBranchNK)
    , Show (getType 'PatternNK)
    , Show (getType 'TypeNK)
    , Show (TypeRef p)
    , Show (CtorRef p)
    , Show (VarRef p)
    ) =>
    Show (AST p getType kind)


mapAll ::
    (TypeRef p1 -> TypeRef p2)
    -> (CtorRef p1 -> CtorRef p2)
    -> (VarRef p1 -> VarRef p2)
    -> (forall kind. getType1 kind -> getType2 kind)
    -> (forall kind.
        AST p1 getType1 kind
        -> AST p2 getType2 kind
        )
mapAll ftyp fctor fvar fast = \case
    TypeRef_ r -> TypeRef_ (ftyp r)
    CtorRef_ r -> CtorRef_ (fctor r)
    VarRef_ r -> VarRef_ (fvar r)

    -- Module
    Module c h d i b -> Module c (fmap fast h) d (fmap (fmap $ fmap fast) i) (fast b)
    ImportMethod as exp -> ImportMethod as (fmap fast exp)
    ModuleListing lst -> ModuleListing lst
    ModuleHeader st n s e -> ModuleHeader st n s (fmap (fmap fast) e)
    ModuleBody ds -> ModuleBody (fmap (fmap fast) ds)

    -- Declaration
    Definition name args c e -> Definition (fast name) (fmap (fmap fast) args) c (fast e)
    TypeAnnotation name t -> TypeAnnotation name (fmap fast t)
    CommonDeclaration d -> CommonDeclaration (fast d)
    Datatype nameWithArgs ctors -> Datatype nameWithArgs (fmap (fmap fast) ctors)
    TypeAlias c nameWithArgs t -> TypeAlias c nameWithArgs (fmap fast t)
    PortAnnotation name c t -> PortAnnotation name c (fast t)
    PortDefinition_until_0_16 name c e -> PortDefinition_until_0_16 name c (fast e)
    Fixity_until_0_18 a c n c' name -> Fixity_until_0_18 a c n c' (fast name)
    Fixity a n op name -> Fixity a n op name

    -- Expressions
    Unit c -> Unit c
    Literal l -> Literal l
    VarExpr var -> VarExpr (fast var)
    App first rest ml -> App (fast first) (fmap (fmap fast) rest) ml
    Unary op e -> Unary op (fast e)
    Binops first ops ml -> Binops (fast first) (fmap (bimap fast fast) ops) ml
    Parens e -> Parens (fmap fast e)
    ExplicitList terms c ml -> ExplicitList (fmap fast terms) c ml
    Range left right -> Range (fmap fast left) (fmap fast right)
    Tuple terms ml -> Tuple (fmap (fmap fast) terms) ml
    TupleFunction n -> TupleFunction n
    Record base fields c ml -> Record base (fmap (fmap fast) fields) c ml
    Access e field -> Access (fast e) field
    AccessFunction field -> AccessFunction field
    Lambda args c e ml -> Lambda (fmap (fmap fast) args) c (fast e) ml
    If cond elsifs els -> If (fmap fast cond) (fmap (fmap $ fmap fast) elsifs) (fmap fast els)
    Let decls c e -> Let (fmap fast decls) c (fast e)
    LetCommonDeclaration d -> LetCommonDeclaration (fast d)
    LetComment c -> LetComment c
    Case (cond, ml) branches -> Case (fmap fast cond, ml) (fmap fast branches)
    CaseBranch c1 c2 c3 p e -> CaseBranch c1 c2 c3 (fast p) (fast e)
    GLShader s -> GLShader s

    -- Patterns
    Anything -> Anything
    UnitPattern c -> UnitPattern c
    LiteralPattern l -> LiteralPattern l
    VarPattern l -> VarPattern l
    OpPattern s -> OpPattern s
    DataPattern ctor pats -> DataPattern (fast ctor) (fmap (fmap fast) pats)
    PatternParens pat -> PatternParens (fmap fast pat)
    TuplePattern pats -> TuplePattern (fmap (fmap fast) pats)
    EmptyListPattern c -> EmptyListPattern c
    ListPattern pats -> ListPattern (fmap (fmap fast) pats)
    ConsPattern first rest -> ConsPattern (fmap fast first) (fmap fast rest)
    EmptyRecordPattern c -> EmptyRecordPattern c
    RecordPattern fields -> RecordPattern fields
    Alias pat name -> Alias (fmap fast pat) name

    -- Types
    UnitType c -> UnitType c
    TypeVariable name -> TypeVariable name
    TypeConstruction name args forceMultiline -> TypeConstruction (fmap fast name) (fmap (fmap fast) args) forceMultiline
    TypeParens typ -> TypeParens (fmap fast typ)
    TupleType typs forceMultiline -> TupleType (fmap (fmap fast) typs) forceMultiline
    RecordType base fields c ml -> RecordType base (fmap (fmap fast) fields) c ml
    FunctionType first rest ml -> FunctionType (fmap fast first) (fmap fast rest) ml

instance I.HFunctor (AST p) where
    -- TODO: it's probably worth making an optimized version of this
    hmap = mapAll id id id

instance I.HFoldable (AST p) where
    hFoldMap f = \case
      TypeRef_ _ -> mempty
      CtorRef_ _ -> mempty
      VarRef_ _ -> mempty
      Module _ header _ imports body -> foldMap f header <> foldMap (foldMap $ foldMap f) imports <> f body
      ImportMethod _ exp -> foldMap f exp
      ModuleListing _ -> mempty
      ModuleHeader _ _ _ exports -> foldMap (foldMap f) exports
      ModuleBody defs -> foldMap (foldMap f) defs
      Definition pat args _ expr -> f pat <> foldMap (f . extract) args <> f expr
      TypeAnnotation _ typ -> f (extract typ)
      CommonDeclaration def -> f def
      Datatype _ tags -> foldMap (foldMap f) tags
      TypeAlias _ _ typ -> f (extract typ)
      PortAnnotation _ _ typ -> f typ
      PortDefinition_until_0_16 _ _ typ -> f typ
      Fixity_until_0_18 _ _ _ _ ref -> f ref
      Fixity {} -> mempty
      Unit _ -> mempty
      Literal _ -> mempty
      VarExpr var -> f var
      App first rest _ -> f first <> foldMap (f . extract) rest
      Unary _ expr -> f expr
      Binops first rest _ -> f first <> foldMap (bifoldMap f f) rest
      Parens term -> f (extract term)
      ExplicitList terms _ _ -> foldMap f terms
      Range a b -> f (extract a) <> f (extract b)
      Tuple terms _ -> foldMap (f . extract) terms
      TupleFunction _ -> mempty
      Record _ fields _ _ -> foldMap (foldMap f) fields
      Access term _ -> f term
      AccessFunction _ -> mempty
      Lambda pats _ expr _ -> foldMap (f . extract) pats <> f expr
      If first rest last -> foldMap f first <> foldMap (foldMap f . extract) rest <> f (extract last)
      Let defs _ expr -> foldMap f defs <> f expr
      LetCommonDeclaration def -> f def
      LetComment _ -> mempty
      Case pred branches -> f (extract $ fst pred) <> foldMap f branches
      CaseBranch _ _ _ pat expr -> f pat <> f expr
      GLShader _ -> mempty
      Anything -> mempty
      UnitPattern _ -> mempty
      LiteralPattern _ -> mempty
      VarPattern _ -> mempty
      OpPattern _ -> mempty
      DataPattern ctor args -> f ctor <> foldMap (f . extract) args
      PatternParens pat -> f (extract pat)
      TuplePattern pats -> foldMap (f . extract) pats
      EmptyListPattern _ -> mempty
      ListPattern pats -> foldMap (f . extract) pats
      ConsPattern first rest -> f (extract first) <> foldMap f rest
      EmptyRecordPattern _ -> mempty
      RecordPattern _ -> mempty
      Alias pat _ -> f (extract pat)
      UnitType _ -> mempty
      TypeVariable _ -> mempty
      TypeConstruction tc args _ -> foldMap f tc <> foldMap (f . extract) args
      TypeParens t -> f $ extract t
      TupleType ne _ -> foldMap (f . extract) ne
      RecordType _ fs _ _ -> foldMap (foldMap f) fs
      FunctionType ft se _ -> f (extract ft) <> foldMap f se


--
-- Recursion schemes
--


topDownReferencesWithContext ::
    forall
        context ns
        p2
        ann kind.
    Functor ann =>
    Coapplicative ann =>
    ASTParameters p2 =>
    (LocalName -> context -> context) -- TODO: since the caller typically passes a function that builds a Map or Set, this could be optimized by taking `List (LocalName)` instead of one at a time
    -> (context -> (ns, UppercaseIdentifier) -> TypeRef p2)
    -> (context -> (ns, UppercaseIdentifier) -> CtorRef p2)
    -> (context -> Ref ns -> VarRef p2)
    -> context
    -> I.Fix2 ann (AST (VariableNamespace ns)) kind
    -> I.Fix2 ann (AST p2) kind
topDownReferencesWithContext defineLocal fType fCtor fVar initialContext initialAst =
    let
        namesFromPattern' ::
            forall p kind'. -- We actually only care about PatternNK' here
            AST p (Const [LocalName]) kind'
            -> Const [LocalName] kind'
        namesFromPattern' = \case
            Anything -> mempty
            UnitPattern _ -> mempty
            LiteralPattern _ -> mempty
            VarPattern l -> Const $ pure $ VarName l
            OpPattern _ -> mempty
            DataPattern _ args -> foldMap extract args
            PatternParens p -> extract p
            TuplePattern ps -> foldMap extract ps
            EmptyListPattern _ -> mempty
            ListPattern ps -> foldMap extract ps
            ConsPattern p ps -> extract p <> fold ps
            EmptyRecordPattern _ -> mempty
            RecordPattern ps -> Const $ fmap (VarName . extract) ps
            Alias p name -> extract p <> Const (pure $ VarName $ extract name)

        namesFromPattern ::
            Coapplicative ann' =>
            I.Fix2 ann' (AST p) kind'
            -> [LocalName]
        namesFromPattern =
            getConst . I.fold2 (namesFromPattern' . extract)

        namesFrom ::
            Coapplicative ann' =>
            I.Fix2 ann' (AST p) kind'
            -> [LocalName]
        namesFrom decl =
            case extract $ I.unFix2 decl of
                Definition p _ _ _ -> namesFromPattern p
                TypeAnnotation _ _ -> mempty

                CommonDeclaration d -> namesFrom d
                Datatype (C _ (NameWithArgs name _)) tags ->
                    TypeName name
                    : fmap (\(NameWithArgs name _) -> CtorName name) (toList tags)
                TypeAlias _ (C _ (NameWithArgs name _)) _ -> [TypeName name]
                PortAnnotation (C _ name) _ _ -> [VarName name]
                PortDefinition_until_0_16 (C _ name) _ _ -> [VarName name]
                Fixity_until_0_18 _ _ _ _ _ -> []
                Fixity _ _ _ _ -> []

                LetCommonDeclaration d -> namesFrom d
                LetComment _ -> mempty

        newDefinitionsAtNode ::
            forall kind'.
            AST (VariableNamespace ns)
                (I.Fix2 ann (AST (VariableNamespace ns)))
                kind'
            -> [LocalName]
        newDefinitionsAtNode node =
            case node of
                Module _ _ _ _ body ->
                    newDefinitionsAtNode (extract $ I.unFix2 body)

                ModuleBody decls ->
                    foldMap (foldMap namesFrom) decls

                CommonDeclaration d ->
                    newDefinitionsAtNode (extract $ I.unFix2 d)

                Definition first rest _ _ ->
                    foldMap namesFromPattern (first : fmap extract rest)

                Lambda args _ _ _ ->
                    foldMap (namesFromPattern . extract) args

                Let decls _ _ ->
                    foldMap namesFrom decls

                LetCommonDeclaration d ->
                    newDefinitionsAtNode (extract $ I.unFix2 d)

                CaseBranch _ _ _ p _ ->
                    namesFromPattern p

                -- TODO: actually implement this for all node types
                _ -> []

        step ::
            forall kind'.
            context
            -> AST (VariableNamespace ns)
                (I.Fix2 ann (AST (VariableNamespace ns)))
                kind'
            -> AST p2
                (Compose
                    ((,) context)
                    (I.Fix2 ann (AST (VariableNamespace ns)))
                )
                kind'
        step context node =
            let
                context' = foldl (flip defineLocal) context (newDefinitionsAtNode node)
            in
            mapAll (fType context') (fCtor context') (fVar context') id
                $ I.hmap (Compose . (,) context') node
    in
    I.unfold2
        (\(Compose (context, ast)) -> step context <$> I.unFix2 ast)
        (Compose (initialContext, initialAst))
