{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import Data.Binary

import qualified AST.Expression.General as Expression
import qualified AST.Type as Type
import qualified Reporting.Annotation as A


-- DECLARATIONS

data Declaration
    = Definition Expression.Def
    | Datatype String [String] [(String, [Type.Raw])]
    | TypeAlias String [String] Type.Raw
    | Port SourcePort
    | Fixity Assoc Int String


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


data SourceDecl
    = Comment String
    | Decl (A.Located Declaration)


-- PORTS

data SourcePort
    = PortAnnotation String Type.Raw
    | PortDefinition String Expression.Expr
