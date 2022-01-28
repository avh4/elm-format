{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Structure
    ( ASTNS, ASTNS1
    , bottomUpReferences
    , mapNs
    ) where


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
