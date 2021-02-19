module ElmFormat.AST.PublicAST.MaybeF where


import Data.Coapplicative
data MaybeF f a
    = JustF (f a)
    | NothingF a
    deriving (Functor)

instance Prelude.Foldable f => Prelude.Foldable (MaybeF f) where
    foldMap f (JustF fa) = Prelude.foldMap f fa
    foldMap f (NothingF a) = f a

instance Traversable f => Traversable (MaybeF f) where
    traverse f (JustF fa) = JustF <$> traverse f fa
    traverse f (NothingF a) = NothingF <$> f a

instance Coapplicative f => Coapplicative (MaybeF f) where
    extract (JustF fa) = extract fa
    extract (NothingF a) = a
