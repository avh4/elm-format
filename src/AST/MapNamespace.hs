{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module AST.MapNamespace where

import AST.Declaration
import AST.Expression
import AST.MapExpr
import AST.Variable
import AST.V0_16


class MapNamespace a where
    mapNamespace :: ([UppercaseIdentifier] -> [UppercaseIdentifier]) -> a -> a


instance MapNamespace Expr' where
    mapNamespace f expr =
        case expr of
            VarExpr var ->
                VarExpr (mapNamespace f var)

            -- TODO: map references in patterns

            _ ->
                mapExpr (mapNamespace f) expr


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


instance (MapNamespace a, Functor f) => MapNamespace (f a) where
    mapNamespace f = fmap (mapNamespace f)
