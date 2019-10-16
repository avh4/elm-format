{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module ElmFormat.Mapping where

import AST.V0_16

import qualified Reporting.Annotation as A


class MapNamespace ns1 ns2 a1 a2 where
    mapNamespace :: (ns1 -> ns2) -> a1 -> a2


instance MapNamespace a b t1 t2 => MapNamespace a b (A.Annotated ann t1) (A.Annotated ann t2) where
    mapNamespace f (A.A ann t) = A.A ann (mapNamespace f t)


instance MapNamespace a b t1 t2 => MapNamespace a b (Commented t1) (Commented t2) where
    mapNamespace f (Commented pre t post) = Commented pre (mapNamespace f t) post


instance MapNamespace a b t1 t2 => MapNamespace a b (PreCommented t1) (PreCommented t2) where
    mapNamespace f (c, t) = (c, mapNamespace f t)


instance MapNamespace a b t1 t2 => MapNamespace a b (List t1) (List t2) where
    mapNamespace f ts = fmap (mapNamespace f) ts


instance MapNamespace a b (Type' a) (Type' b) where
    mapNamespace = fmap



class MapReferences ns1 ns2 a1 a2 where
    mapReferences ::
        ((ns1, UppercaseIdentifier) -> (ns2, UppercaseIdentifier))
        -> ((ns1, LowercaseIdentifier) -> (ns2, LowercaseIdentifier))
        -> a1
        -> a2
    mapReferences' ::
        ( (ns1, UppercaseIdentifier) -> (ns2, UppercaseIdentifier)
        , (ns1, LowercaseIdentifier) -> (ns2, LowercaseIdentifier)
        )
        -> a1
        -> a2
    mapReferences' (fu, fl) = mapReferences fu fl


instance MapReferences a b t1 t2 => MapReferences a b (A.Annotated ann t1) (A.Annotated ann t2) where
    mapReferences fu fl (A.A ann t) = A.A ann (mapReferences fu fl t)


instance MapReferences a b t1 t2 => MapReferences a b (Commented t1) (Commented t2) where
    mapReferences fu fl (Commented pre t post) = Commented pre (mapReferences fu fl t) post


instance MapReferences a b t1 t2 => MapReferences a b (PreCommented t1) (PreCommented t2) where
    mapReferences fu fl (c, t) = (c, mapReferences fu fl t)


instance MapReferences a b t1 t2 => MapReferences a b (WithEol t1) (WithEol t2) where
    mapReferences fu fl (WithEol t eol) = WithEol (mapReferences fu fl t) eol


instance MapReferences a b t1 t2 => MapReferences a b (Pair x t1) (Pair x t2) where
    mapReferences fu fl (Pair k v ml) = Pair k (mapReferences fu fl v) ml


instance MapReferences a b t1 t2 => MapReferences a b (List t1) (List t2) where
    mapReferences fu fl ts = fmap (mapReferences fu fl) ts


instance MapReferences a b (TypeConstructor a) (TypeConstructor b) where
    mapReferences fu _ (NamedConstructor name) = NamedConstructor (fu name)
    mapReferences _ _ (TupleConstructor n) = TupleConstructor n


instance MapReferences a b (Type' a) (Type' b) where
    mapReferences fu fl = \case
        UnitType c -> UnitType c
        TypeVariable name -> TypeVariable name
        TypeConstruction ctor args -> TypeConstruction (mapReferences fu fl ctor) (mapReferences fu fl args)
        TypeParens t -> TypeParens (mapReferences fu fl t)
        TupleType ts -> TupleType (mapReferences fu fl ts)
        RecordType base fields c ml -> RecordType base (mapReferences fu fl fields) c ml
        FunctionType first rest ml -> FunctionType (mapReferences fu fl first) (mapReferences fu fl rest) ml



-- TODO: add MapAnnotation
