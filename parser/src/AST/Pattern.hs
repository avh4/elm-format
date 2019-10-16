{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AST.Pattern where

import AST.V0_16
import ElmFormat.Mapping

import qualified Reporting.Annotation as A


type Pattern ns =
    A.Located (Pattern' ns)


data Pattern' ns
    = Anything
    | UnitPattern Comments
    | Literal Literal
    | VarPattern LowercaseIdentifier
    | OpPattern SymbolIdentifier
    | Data (ns, UppercaseIdentifier) [(Comments, Pattern ns)]
    | PatternParens (Commented (Pattern ns))
    | Tuple [Commented (Pattern ns)]
    | EmptyListPattern Comments
    | List [Commented (Pattern ns)]
    | ConsPattern
        { first :: WithEol (Pattern ns)
        , rest :: Sequence (Pattern ns)
        }
    | EmptyRecordPattern Comments
    | Record [Commented LowercaseIdentifier]
    | Alias (Pattern ns, Comments) (Comments, LowercaseIdentifier)
    deriving (Eq, Show, Functor)


instance MapNamespace a b (Pattern' a) (Pattern' b) where
    mapNamespace = fmap


instance MapReferences a b (Pattern' a) (Pattern' b) where
    mapReferences fu fl = \case
        Anything -> Anything
        UnitPattern c -> UnitPattern c
        Literal l -> Literal l
        VarPattern l -> VarPattern l
        OpPattern o -> OpPattern o
        Data ctor args -> Data (fu ctor) (mapReferences fu fl args)
        PatternParens p -> PatternParens (mapReferences fu fl p)
        Tuple ps -> Tuple (mapReferences fu fl ps)
        EmptyListPattern c -> EmptyListPattern c
        List ps -> List (mapReferences fu fl ps)
        ConsPattern first rest -> ConsPattern (mapReferences fu fl first) (mapReferences fu fl rest)
        EmptyRecordPattern c -> EmptyRecordPattern c
        Record fs -> Record fs
        Alias (p, c) as -> Alias (mapReferences fu fl p, c) as
