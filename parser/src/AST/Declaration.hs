{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import Data.Binary

import qualified AST.Expression.General as General
import qualified AST.Expression.Source as Source
import qualified AST.Type as Type
import qualified Reporting.Annotation as A


-- DECLARATIONS

data Declaration' port def tipe expr
    = Definition def
    | Datatype String [String] [(String, [tipe])]
    | TypeAlias String [String] tipe
    | Port port
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

type SourceDecl' =
  Declaration' SourcePort Source.Def Type.Raw Source.Expr


data SourceDecl
    = Comment String
    | Decl (A.Located SourceDecl')


-- PORTS

data SourcePort
    = PortAnnotation String Type.Raw
    | PortDefinition String Source.Expr
