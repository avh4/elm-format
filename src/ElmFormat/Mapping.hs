{-# LANGUAGE FlexibleInstances #-}

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


-- TODO: add MapAnnotation
