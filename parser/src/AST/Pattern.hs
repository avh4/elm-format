{-# LANGUAGE FlexibleInstances #-}

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
