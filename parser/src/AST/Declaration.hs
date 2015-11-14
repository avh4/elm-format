{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import AST.V0_15
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
    | Fixity Assoc Int (Commented Var.Ref)
    deriving (Eq, Show)


stripRegion :: Declaration -> Declaration
stripRegion d =
    case d of
        Definition e ->
            Definition $ Expression.stripRegion e
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
    = Comment String
    | Decl (A.Located Declaration)
    deriving (Eq, Show)


stripRegion' :: Decl -> Decl
stripRegion' d =
    case d of
        Decl d' ->
            Decl $ A.stripRegion $ A.map stripRegion d'
        _ -> d
