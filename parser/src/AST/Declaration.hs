{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import qualified AST.Expression as Expression
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


-- DECLARATIONS

data Declaration
    = Definition Expression.Def
    | Datatype String [String] [(String, [Type.Type])]
    | TypeAlias String [String] Type.Type
    | PortAnnotation String Type.Type
    | PortDefinition String Expression.Expr
    | Fixity Assoc Int Var.Ref


-- INFIX STUFF

data Assoc = L | N | R
    deriving (Eq)


assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


-- DECLARATION PHASES


data Decl
    = Comment String
    | Decl (A.Located Declaration)
