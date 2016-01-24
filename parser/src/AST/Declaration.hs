{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import AST.V0_16
import qualified AST.Expression as Expression
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


-- DECLARATIONS

data Declaration
    = Definition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments Expression.Expr
    | TypeAnnotation (Var.Ref, Comments) (Comments, Type)
    | Datatype
        (Commented (String, [(Comments, String)]))
        [Commented (String, [(Comments, Type)])]
        (Comments, (String, [(Comments, Type)]))
    | TypeAlias Comments
        (Commented (String, [(Comments, String)]))
        (Comments, Type)
    | PortAnnotation (Commented String) Comments Type
    | PortDefinition (Commented String) Comments Expression.Expr
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
