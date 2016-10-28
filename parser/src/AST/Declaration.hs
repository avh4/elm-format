{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import AST.V0_16
import qualified AST.Expression as Expression
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


-- DECLARATIONS

data Declaration
    = Definition Pattern.Pattern [PreCommented Pattern.Pattern] Comments Expression.Expr
    | TypeAnnotation (PostCommented Var.Ref) (PreCommented Type)
    | Datatype
        (Commented (UppercaseIdentifier, [PreCommented LowercaseIdentifier]))
        [Commented (UppercaseIdentifier, [PreCommented Type])]
        (PreCommented (UppercaseIdentifier, [PreCommented Type]))
    | TypeAlias Comments
        (Commented (UppercaseIdentifier, [PreCommented LowercaseIdentifier]))
        (PreCommented Type)
    | PortAnnotation (Commented LowercaseIdentifier) Comments Type
    | PortDefinition (Commented LowercaseIdentifier) Comments Expression.Expr
    | Fixity Assoc Comments Int Comments Var.Ref
    deriving (Eq, Show)


instance A.Strippable Declaration where
  stripRegion d =
    case d of
        Definition a b c e ->
            Definition (A.stripRegion a) b c (A.stripRegion $ A.map A.stripRegion e)
        _ -> d

-- INFIX STUFF

data Assoc = L | N | R
    deriving (Eq, Show)


assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


-- DECLARATION PHASES


data Decl
    = DocComment String
    | BodyComment Comment
    | Decl (A.Located Declaration)
    deriving (Eq, Show)


instance A.Strippable Decl where
  stripRegion d =
    case d of
        Decl d' ->
            Decl $ A.stripRegion $ A.map A.stripRegion d'
        _ -> d
