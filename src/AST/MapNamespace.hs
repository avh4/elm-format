{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AST.MapNamespace where

import AST.Declaration
import AST.Expression
import AST.Variable
import AST.V0_16
import Data.Fix


class MapNamespace a where
    mapNamespace :: ([UppercaseIdentifier] -> [UppercaseIdentifier]) -> a -> a


instance MapNamespace (Fix (AnnotatedExpression ann)) where
    mapNamespace f (Fix (AE e)) =
        Fix $ AE $ mapNamespace f e


instance MapNamespace e => MapNamespace (Expression e) where
    mapNamespace f expr =
        -- TODO: map references in patterns
        case expr of
            Unit _ -> expr
            AST.Expression.Literal _ -> expr
            VarExpr var -> VarExpr (mapNamespace f var)

            App f' args multiline ->
                App (mapNamespace f f') (mapNamespace f args) multiline
            Unary op e ->
                Unary op (mapNamespace f e)
            Binops left restOps multiline ->
                Binops (mapNamespace f left) (mapNamespace f restOps) multiline
            Parens e ->
                Parens (mapNamespace f e)
            ExplicitList terms' post multiline ->
                ExplicitList (mapNamespace f terms') post multiline
            Range e1 e2 multiline ->
                Range (mapNamespace f e1) (mapNamespace f e2) multiline
            AST.Expression.Tuple es multiline ->
                AST.Expression.Tuple (mapNamespace f es) multiline
            TupleFunction _ -> expr
            AST.Expression.Record b fs post multiline ->
                AST.Expression.Record b (mapNamespace f fs) post multiline
            Access e field' ->
                Access (mapNamespace f e) field'
            AccessFunction _ -> expr
            Lambda params pre body multi ->
                Lambda params pre (mapNamespace f body) multi
            If c1 elseIfs els ->
                If (mapNamespace f c1) (mapNamespace f elseIfs) (mapNamespace f els)
            Let decls pre body ->
                Let (mapNamespace f decls) pre body
            Case (cond, m) branches ->
                Case (mapNamespace f cond, m) (mapNamespace f branches)
            GLShader _ -> expr


instance MapNamespace (Fix Expression) where
    mapNamespace f (Fix e) =
        Fix $ mapNamespace f e


instance MapNamespace Type' where
    mapNamespace f typ =
        case typ of
            UnitType comments ->
                UnitType comments

            TypeVariable name ->
                TypeVariable name

            TypeConstruction ctor args ->
                TypeConstruction (mapNamespace f ctor) (mapNamespace f args)

            TypeParens nested ->
                TypeParens (mapNamespace f nested)

            TupleType entries ->
                TupleType (mapNamespace f entries)

            RecordType base fields trailingComments forceMultiline ->
                RecordType base (mapNamespace f fields) trailingComments forceMultiline

            FunctionType first rest forceMultiline ->
                FunctionType (mapNamespace f first) (fmap (\(a, b, x, d) -> (a, b, mapNamespace f x, d)) rest) forceMultiline


instance MapNamespace TypeConstructor where
    mapNamespace f ctor =
        case ctor of
            NamedConstructor namespace name ->
                NamedConstructor (f namespace) name

            TupleConstructor n ->
                TupleConstructor n


instance MapNamespace Declaration where
    mapNamespace f decl =
        case decl of
            -- TODO: map references in patterns
            Definition first rest comments expr ->
                Definition first rest comments (mapNamespace f expr)

            TypeAnnotation name typ ->
                TypeAnnotation name (mapNamespace f typ)

            Datatype nameWithArgs tags ->
                Datatype nameWithArgs (fmap (\(name, args) -> (name, fmap (mapNamespace f) args)) tags)

            TypeAlias comments name typ ->
                TypeAlias comments name (mapNamespace f typ)

            PortAnnotation name comments typ ->
                PortAnnotation name comments (mapNamespace f typ)

            PortDefinition name comments expr ->
                PortDefinition name comments (mapNamespace f expr)

            _ -> decl


instance MapNamespace Ref where
    mapNamespace f ref =
        case ref of
            VarRef namespace name -> VarRef (f namespace) name
            TagRef namespace name -> TagRef (f namespace) name
            OpRef name -> OpRef name


instance {-# OVERLAPPABLE #-} (MapNamespace a, Functor f) => MapNamespace (f a) where
    mapNamespace f = fmap (mapNamespace f)
