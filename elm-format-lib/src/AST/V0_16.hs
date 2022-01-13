{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
    deriving (Eq, Show, Functor)

instance Foldable OpenCommentedList where
    foldMap f (OpenCommentedList rest last) = foldMap (f . extract) rest <> (f . extract) last

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
    deriving (Show, Eq, Functor)

mapPair :: (a1 -> a2) -> (b1 -> b2) -> Pair a1 b1 -> Pair a2 b2
mapPair fa fb (Pair k v fm) =
    Pair (fa <$> k) (fb <$> v) fm


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
    deriving (Eq, Show, Functor)
instance Foldable (NameWithArgs name) where
    foldMap f (NameWithArgs _ args) = foldMap (f . extract) args


data TypeConstructor ctorRef
    = NamedConstructor ctorRef
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show, Functor)


data BinopsClause varRef expr =
    BinopsClause Comments varRef Comments expr
    deriving (Eq, Show, Functor)

instance Bifunctor BinopsClause where
    bimap fvr fe = \case
        BinopsClause c1 vr c2 e -> BinopsClause c1 (fvr vr) c2 (fe e)


data IfClause e =
    IfClause (C2 'BeforeTerm 'AfterTerm e) (C2 'BeforeTerm 'AfterTerm e)
    deriving (Eq, Show, Functor)


data TopLevelStructure a
    = DocComment Markdown.Blocks
    | BodyComment Comment
    | Entry a
    deriving (Eq, Show, Functor)

instance Foldable TopLevelStructure where
    foldMap _ (DocComment _) = mempty
    foldMap _ (BodyComment _) = mempty
    foldMap f (Entry a) = f a


data LocalName
    = TypeName UppercaseIdentifier
    | CtorName UppercaseIdentifier
    | VarName LowercaseIdentifier
    deriving (Eq, Ord, Show)


data NodeKind
    = TopLevelNK
    | CommonDeclarationNK
    | TopLevelDeclarationNK
    | ExpressionNK
    | LetDeclarationNK
    | CaseBranchNK
    | PatternNK
    | TypeNK


data AST typeRef ctorRef varRef (getType :: NodeKind -> Type) (kind :: NodeKind) where

    TopLevel ::
        [TopLevelStructure (getType 'TopLevelDeclarationNK)]
        -> AST typeRef ctorRef varRef getType 'TopLevelNK

    --
    -- Declarations
    --

    Definition ::
        getType 'PatternNK
        -> [C1 'BeforeTerm (getType 'PatternNK)]
        -> Comments
        -> getType 'ExpressionNK
        -> AST typeRef ctorRef varRef getType 'CommonDeclarationNK
    TypeAnnotation ::
        C1 'AfterTerm (Ref ())
        -> C1 'BeforeTerm (getType 'TypeNK)
        -> AST typeRef ctorRef varRef getType 'CommonDeclarationNK
    CommonDeclaration ::
        getType 'CommonDeclarationNK
        -> AST typeRef ctorRef varRef getType 'TopLevelDeclarationNK
    Datatype ::
        { nameWithArgs :: C2 'BeforeTerm 'AfterTerm (NameWithArgs UppercaseIdentifier LowercaseIdentifier)
        , tags :: OpenCommentedList (NameWithArgs UppercaseIdentifier (getType 'TypeNK))
        }
        -> AST typeRef ctorRef varRef getType 'TopLevelDeclarationNK
    TypeAlias ::
        Comments
        -> C2 'BeforeTerm 'AfterTerm (NameWithArgs UppercaseIdentifier LowercaseIdentifier)
        -> C1 'BeforeTerm (getType 'TypeNK)
        -> AST typeRef ctorRef varRef getType 'TopLevelDeclarationNK
    PortAnnotation ::
        C2 'BeforeTerm 'AfterTerm LowercaseIdentifier
        -> Comments
        -> getType 'TypeNK
        -> AST typeRef ctorRef varRef getType 'TopLevelDeclarationNK
    PortDefinition_until_0_16 ::
        C2 'BeforeTerm 'AfterTerm LowercaseIdentifier
        -> Comments
        -> getType 'ExpressionNK
        -> AST typeRef ctorRef varRef getType 'TopLevelDeclarationNK
    Fixity_until_0_18 ::
        Assoc
        -> Comments
        -> Int
        -> Comments
        -> varRef
        -> AST typeRef ctorRef varRef getType 'TopLevelDeclarationNK
    Fixity ::
        C1 'BeforeTerm Assoc
        -> C1 'BeforeTerm Int
        -> C2 'BeforeTerm 'AfterTerm SymbolIdentifier
        -> C1 'BeforeTerm LowercaseIdentifier
        -> AST typeRef ctorRef varRef getType 'TopLevelDeclarationNK

    --
    -- Expressions
    --

    Unit ::
        Comments
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    Literal ::
        LiteralValue
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    VarExpr ::
        varRef
        -> AST typeRef ctorRef varRef getType 'ExpressionNK

    App ::
        getType 'ExpressionNK
        -> [C1 'BeforeTerm (getType 'ExpressionNK)]
        -> FunctionApplicationMultiline
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    Unary ::
        UnaryOperator
        -> getType 'ExpressionNK
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    Binops ::
        getType 'ExpressionNK
        -> List (BinopsClause varRef (getType 'ExpressionNK)) -- Non-empty
        -> Bool
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    Parens ::
        C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)
        -> AST typeRef ctorRef varRef getType 'ExpressionNK

    ExplicitList ::
        { terms :: Sequence (getType 'ExpressionNK)
        , trailingComments_el :: Comments
        , forceMultiline_el :: ForceMultiline
        }
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    Range ::
        C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)
        -> C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)
        -> Bool
        -> AST typeRef ctorRef varRef getType 'ExpressionNK

    Tuple ::
        [C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK)]
        -> Bool
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    TupleFunction ::
        Int -- will be 2 or greater, indicating the number of elements in the tuple
        -> AST typeRef ctorRef varRef getType 'ExpressionNK

    Record ::
        { base_r :: Maybe (C2 'BeforeTerm 'AfterTerm LowercaseIdentifier)
        , fields_r :: Sequence (Pair LowercaseIdentifier (getType 'ExpressionNK))
        , trailingComments_r :: Comments
        , forceMultiline_r :: ForceMultiline
        }
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    Access ::
        getType 'ExpressionNK
        -> LowercaseIdentifier
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    AccessFunction ::
        LowercaseIdentifier
        -> AST typeRef ctorRef varRef getType 'ExpressionNK

    Lambda ::
        [C1 'BeforeTerm (getType 'PatternNK)]
        -> Comments
        -> getType 'ExpressionNK
        -> Bool
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    If ::
        IfClause (getType 'ExpressionNK)
        -> [C1 'BeforeTerm (IfClause (getType 'ExpressionNK))]
        -> C1 'BeforeTerm (getType 'ExpressionNK)
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    Let ::
        [getType 'LetDeclarationNK]
        -> Comments
        -> getType 'ExpressionNK
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    LetCommonDeclaration ::
        getType 'CommonDeclarationNK
        -> AST typeRef ctorRef varRef getType 'LetDeclarationNK
    LetComment ::
        Comment
        -> AST typeRef ctorRef varRef getType 'LetDeclarationNK
    Case ::
        (C2 'BeforeTerm 'AfterTerm (getType 'ExpressionNK), Bool)
        -> [getType 'CaseBranchNK]
        -> AST typeRef ctorRef varRef getType 'ExpressionNK
    CaseBranch ::
        { beforePattern :: Comments
        , beforeArrow :: Comments
        , afterArrow :: Comments
        , pattern :: getType 'PatternNK
        , body :: getType 'ExpressionNK
        }
        -> AST typeRef ctorRef varRef getType 'CaseBranchNK

    -- for type checking and code gen only
    GLShader ::
        String
        -> AST typeRef ctorRef varRef getType 'ExpressionNK


    --
    -- Patterns
    --

    Anything ::
        AST typeRef ctorRef varRef getType 'PatternNK
    UnitPattern ::
        Comments
        -> AST typeRef ctorRef varRef getType 'PatternNK
    LiteralPattern ::
        LiteralValue
        -> AST typeRef ctorRef varRef getType 'PatternNK
    VarPattern ::
        LowercaseIdentifier
        -> AST typeRef ctorRef varRef getType 'PatternNK
    OpPattern ::
        SymbolIdentifier
        -> AST typeRef ctorRef varRef getType 'PatternNK
    DataPattern ::
        ctorRef
        -> [C1 'BeforeTerm (getType 'PatternNK)]
        -> AST typeRef ctorRef varRef getType 'PatternNK
    PatternParens ::
        C2 'BeforeTerm 'AfterTerm (getType 'PatternNK)
        -> AST typeRef ctorRef varRef getType 'PatternNK
    TuplePattern ::
        [C2 'BeforeTerm 'AfterTerm (getType 'PatternNK)]
        -> AST typeRef ctorRef varRef getType 'PatternNK
    EmptyListPattern ::
        Comments
        -> AST typeRef ctorRef varRef getType 'PatternNK
    ListPattern ::
        [C2 'BeforeTerm 'AfterTerm (getType 'PatternNK)]
        -> AST typeRef ctorRef varRef getType 'PatternNK
    ConsPattern ::
        { first_cp :: C0Eol (getType 'PatternNK)
        , rest_cp :: Sequence (getType 'PatternNK)
        }
        -> AST typeRef ctorRef varRef getType 'PatternNK
    EmptyRecordPattern ::
        Comments
        -> AST typeRef ctorRef varRef getType 'PatternNK
    RecordPattern ::
        [C2 'BeforeTerm 'AfterTerm LowercaseIdentifier]
        -> AST typeRef ctorRef varRef getType 'PatternNK
    Alias ::
        C1 'AfterTerm (getType 'PatternNK)
        -> C1 'BeforeTerm LowercaseIdentifier
        -> AST typeRef ctorRef varRef getType 'PatternNK


    --
    -- Types
    --

    UnitType ::
        Comments
        -> AST typeRef ctorRef varRef getType 'TypeNK
    TypeVariable ::
        LowercaseIdentifier
        -> AST typeRef ctorRef varRef getType 'TypeNK
    TypeConstruction ::
        TypeConstructor typeRef
        -> [C1 'BeforeTerm (getType 'TypeNK)]
        -> ForceMultiline
        -> AST typeRef ctorRef varRef getType 'TypeNK
    TypeParens ::
        C2 'BeforeTerm 'AfterTerm (getType 'TypeNK)
        -> AST typeRef ctorRef varRef getType 'TypeNK
    TupleType ::
        [C2Eol 'BeforeTerm 'AfterTerm (getType 'TypeNK)]
        -> ForceMultiline
        -> AST typeRef ctorRef varRef getType 'TypeNK
    RecordType ::
        { base_rt :: Maybe (C2 'BeforeTerm 'AfterTerm LowercaseIdentifier)
        , fields_rt :: Sequence (Pair LowercaseIdentifier (getType 'TypeNK))
        , trailingComments_rt :: Comments
        , forceMultiline_rt :: ForceMultiline
        }
        -> AST typeRef ctorRef varRef getType 'TypeNK
    FunctionType ::
        { first_ft :: C0Eol (getType 'TypeNK)
        , rest_ft :: Sequence (getType 'TypeNK)
        , forceMultiline_ft :: ForceMultiline
        }
        -> AST typeRef ctorRef varRef getType 'TypeNK

deriving instance
    ( Eq typeRef, Eq ctorRef, Eq varRef
    , Eq (getType 'CommonDeclarationNK)
    , Eq (getType 'TopLevelDeclarationNK)
    , Eq (getType 'ExpressionNK)
    , Eq (getType 'LetDeclarationNK)
    , Eq (getType 'CaseBranchNK)
    , Eq (getType 'PatternNK)
    , Eq (getType 'TypeNK)
    ) =>
    Eq (AST typeRef ctorRef varRef getType kind)
deriving instance
    ( Show typeRef, Show ctorRef, Show varRef
    , Show (getType 'CommonDeclarationNK)
    , Show (getType 'TopLevelDeclarationNK)
    , Show (getType 'ExpressionNK)
    , Show (getType 'LetDeclarationNK)
    , Show (getType 'CaseBranchNK)
    , Show (getType 'PatternNK)
    , Show (getType 'TypeNK)
    ) =>
    Show (AST typeRef ctorRef varRef getType kind)


mapAll ::
    (typeRef1 -> typeRef2) -> (ctorRef1 -> ctorRef2) -> (varRef1 -> varRef2)
    -> (forall kind. getType1 kind -> getType2 kind)
    -> (forall kind.
        AST typeRef1 ctorRef1 varRef1 getType1 kind
        -> AST typeRef2 ctorRef2 varRef2 getType2 kind
        )
mapAll ftyp fctor fvar fast = \case
    TopLevel tls -> TopLevel (fmap (fmap fast) tls)

    -- Declaration
    Definition name args c e -> Definition (fast name) (fmap (fmap fast) args) c (fast e)
    TypeAnnotation name t -> TypeAnnotation name (fmap fast t)
    CommonDeclaration d -> CommonDeclaration (fast d)
    Datatype nameWithArgs ctors -> Datatype nameWithArgs (fmap (fmap fast) ctors)
    TypeAlias c nameWithArgs t -> TypeAlias c nameWithArgs (fmap fast t)
    PortAnnotation name c t -> PortAnnotation name c (fast t)
    PortDefinition_until_0_16 name c e -> PortDefinition_until_0_16 name c (fast e)
    Fixity_until_0_18 a c n c' name -> Fixity_until_0_18 a c n c' (fvar name)
    Fixity a n op name -> Fixity a n op name

    -- Expressions
    Unit c -> Unit c
    Literal l -> Literal l
    VarExpr var -> VarExpr (fvar var)
    App first rest ml -> App (fast first) (fmap (fmap fast) rest) ml
    Unary op e -> Unary op (fast e)
    Binops first ops ml -> Binops (fast first) (fmap (bimap fvar fast) ops) ml
    Parens e -> Parens (fmap fast e)
    ExplicitList terms c ml -> ExplicitList (fmap fast terms) c ml
    Range left right ml -> Range (fmap fast left) (fmap fast right) ml
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
    DataPattern ctor pats -> DataPattern (fctor ctor) (fmap (fmap fast) pats)
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
    TypeConstruction name args forceMultiline -> TypeConstruction (fmap ftyp name) (fmap (fmap fast) args) forceMultiline
    TypeParens typ -> TypeParens (fmap fast typ)
    TupleType typs forceMultiline -> TupleType (fmap (fmap fast) typs) forceMultiline
    RecordType base fields c ml -> RecordType base (fmap (fmap fast) fields) c ml
    FunctionType first rest ml -> FunctionType (fmap fast first) (fmap fast rest) ml


instance I.IFunctor (AST typeRef ctorRef varRef) where
    -- TODO: it's probably worth making an optimized version of this
    imap = mapAll id id id



--
-- Recursion schemes
--


topDownReferencesWithContext ::
    forall
        context ns
        typeRef2 ctorRef2 varRef2
        ann kind.
    Functor ann =>
    Coapplicative ann =>
    (LocalName -> context -> context) -- TODO: since the caller typically passes a function that builds a Map or Set, this could be optimized by taking `List (LocalName)` instead of one at a time
    -> (context -> (ns, UppercaseIdentifier) -> typeRef2)
    -> (context -> (ns, UppercaseIdentifier) -> ctorRef2)
    -> (context -> Ref ns -> varRef2)
    -> context
    -> I.Fix ann (AST (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns)) kind
    -> I.Fix ann (AST typeRef2 ctorRef2 varRef2) kind
topDownReferencesWithContext defineLocal fType fCtor fVar initialContext initialAst =
    let
        namesFromPattern' ::
            forall a b c kind'. -- We actually only care about PatternNK' here
            AST a b c (Const [LocalName]) kind'
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
            I.Fix ann' (AST a b c) kind'
            -> [LocalName]
        namesFromPattern =
            getConst . I.cata (namesFromPattern' . extract)

        namesFrom ::
            Coapplicative ann' =>
            I.Fix ann' (AST a b c) kind'
            -> [LocalName]
        namesFrom decl =
            case extract $ I.unFix decl of
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
            AST (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns)
                (I.Fix ann (AST (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns)))
                kind'
            -> [LocalName]
        newDefinitionsAtNode node =
            case node of
                TopLevel decls ->
                    foldMap (foldMap namesFrom) decls

                CommonDeclaration d ->
                    newDefinitionsAtNode (extract $ I.unFix d)

                Definition first rest _ _ ->
                    foldMap namesFromPattern (first : fmap extract rest)

                Lambda args _ _ _ ->
                    foldMap (namesFromPattern . extract) args

                Let decls _ _ ->
                    foldMap namesFrom decls

                LetCommonDeclaration d ->
                    newDefinitionsAtNode (extract $ I.unFix d)

                CaseBranch _ _ _ p _ ->
                    namesFromPattern p

                -- TODO: actually implement this for all node types
                _ -> []

        step ::
            forall kind'.
            context
            -> AST (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns)
                (I.Fix ann (AST (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns)))
                kind'
            -> AST typeRef2 ctorRef2 varRef2
                (Compose
                    ((,) context)
                    (I.Fix ann (AST (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns)))
                )
                kind'
        step context node =
            let
                context' = foldl (flip defineLocal) context (newDefinitionsAtNode node)
            in
            mapAll (fType context') (fCtor context') (fVar context') id
                $ I.imap (Compose . (,) context') node
    in
    I.ana
        (\(Compose (context, ast)) -> step context <$> I.unFix ast)
        (Compose (initialContext, initialAst))
