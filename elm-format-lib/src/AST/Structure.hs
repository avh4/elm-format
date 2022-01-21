{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module AST.Structure
    ( ASTNS, ASTNS1
    , foldReferences
    , bottomUpReferences
    , mapNs
    ) where


import Data.Coapplicative
import Data.Foldable (fold)
import Data.Functor.Const
import AST.V0_16
import qualified Data.Indexed as I


type ASTNS ns =
    AST (VariableNamespace ns)


-- ASTNS1 :: (Type -> Type) -> Type -> NodeKind -> Type
type ASTNS1 annf ns =
    ASTNS ns (I.Fix2 annf (ASTNS ns))


bottomUpReferences ::
    (Functor annf) =>
    (TypeRef p1 -> TypeRef p2)
    -> (CtorRef p1 -> CtorRef p2)
    -> (VarRef p1 -> VarRef p2)
    -> (forall kind.
        I.Fix2 annf (AST p1) kind
        -> I.Fix2 annf (AST p2) kind
       )
bottomUpReferences ftr fcr fvr =
    I.fold2 (I.Fix2 . fmap (mapAll ftr fcr fvr id))


foldReferences ::
    forall a annf p kind.
    (Monoid a, Coapplicative annf) =>
    (TypeRef p -> a) -> (CtorRef p -> a) -> (VarRef p -> a)
    -> I.Fix2 annf (AST p) kind -> a
foldReferences ftype fctor fvar =
    I.foldConst2 (foldNode  . extract)
    where
        -- This is kinda confusing, but we use the Const type constructor to merge all the different NodeKinds into a single type `a`
        -- See http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html for relevant details.
        foldNode :: forall kind'. AST p (Const a) kind' -> a
        foldNode = \case
            TypeRef_ r -> ftype r
            CtorRef_ r -> fctor r
            VarRef_ r -> fvar r

            -- Module
            Module _ h d _ b -> maybe mempty getConst h <> maybe mempty getConst mempty d <> getConst b
            ModuleHeader {} -> mempty
            ModuleBody ds -> foldMap (foldMap getConst) ds

            -- Declarations
            Definition name args _ e -> getConst name <> foldMap (getConst . extract) args <> getConst e
            TypeAnnotation _ t -> getConst $ extract t
            CommonDeclaration d -> getConst d
            Datatype _ ctors -> foldMap (getConst . fold) ctors
            TypeAlias _ _ t -> getConst $ extract t
            PortAnnotation _ _ t -> getConst t
            PortDefinition_until_0_16 _ _ e -> getConst e
            Fixity_until_0_18 _ _ _ _ name -> getConst name
            Fixity _ _ _ _ -> mempty

            -- Expressions
            Unit _ -> mempty
            Literal _ -> mempty
            VarExpr var -> getConst var
            App first rest _ -> getConst first <> foldMap (getConst . extract) rest
            Unary _ e -> getConst e
            Binops first ops _ -> getConst first <> foldMap foldBinopsClause ops
            Parens e -> getConst $ extract e
            ExplicitList terms _ _ -> foldMap getConst terms
            Range left right -> getConst (extract left) <> getConst (extract right)
            Tuple terms _ -> foldMap (getConst . extract) terms
            TupleFunction _ -> mempty
            Record _ fields _ _ -> foldMap (getConst . extract . _value) fields
            Access e _ -> getConst e
            AccessFunction _ -> mempty
            Lambda args _ e _ -> foldMap (getConst . extract) args <> getConst e
            If cond elsifs els -> foldIfClause cond <> foldMap (foldIfClause . extract) elsifs <> getConst (extract els)
            Let defs _ e -> foldMap getConst defs <> getConst e
            LetCommonDeclaration d -> getConst d
            LetComment _ -> mempty
            Case (cond, _) branches -> getConst (extract cond) <> foldMap getConst branches
            CaseBranch _ _ _ p e -> getConst p <> getConst e
            GLShader _ -> mempty

            -- Patterns
            Anything -> mempty
            UnitPattern _ -> mempty
            LiteralPattern _ -> mempty
            VarPattern _ -> mempty
            OpPattern _ -> mempty
            DataPattern ctor args -> getConst ctor <> foldMap (getConst . extract) args
            PatternParens p -> getConst $ extract p
            TuplePattern terms -> foldMap (getConst . extract) terms
            EmptyListPattern _ -> mempty
            ListPattern terms -> foldMap (getConst . extract) terms
            ConsPattern first rest -> getConst (extract first) <> foldMap getConst rest
            EmptyRecordPattern _ -> mempty
            RecordPattern _ -> mempty
            Alias p _ -> getConst $ extract p

            -- Types
            UnitType _ -> mempty
            TypeVariable _ -> mempty
            TypeConstruction name args _ -> foldTypeConstructor name <> foldMap (getConst . extract) args
            TypeParens typ -> getConst $ extract typ
            TupleType terms _ -> foldMap (getConst . extract) terms
            RecordType _ fields _ _ -> foldMap (getConst . extract . _value) fields
            FunctionType first rest _ -> getConst (extract first) <> foldMap getConst rest

        foldTypeConstructor :: TypeConstructor (Const a 'TypeRefNK) -> a
        foldTypeConstructor = \case
            NamedConstructor name -> getConst name
            TupleConstructor _ -> mempty

        foldBinopsClause :: BinopsClause (Const a 'VarRefNK) (Const a 'ExpressionNK) -> a
        foldBinopsClause = \case
            BinopsClause _ op _ e -> getConst op <> getConst e

        foldIfClause :: IfClause (Const a 'ExpressionNK) -> a
        foldIfClause = \case
            IfClause cond els -> getConst (extract cond) <> getConst (extract els)


mapNs ::
    Functor annf =>
    (ns1 -> ns2)
    -> (forall kind.
        I.Fix2 annf (ASTNS ns1) kind
        -> I.Fix2 annf (ASTNS ns2) kind
       )
mapNs f =
    let
        mapTypeRef (ns, u) = (f ns, u)
        mapCtorRef (ns, u) = (f ns, u)
        mapVarRef (VarRef ns l) = VarRef (f ns) l
        mapVarRef (TagRef ns u) = TagRef (f ns) u
        mapVarRef (OpRef op) = OpRef op
    in
    bottomUpReferences mapTypeRef mapCtorRef mapVarRef
