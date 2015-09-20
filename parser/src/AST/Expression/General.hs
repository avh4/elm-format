{-# OPTIONS_GHC -Wall #-}

{-| The Abstract Syntax Tree (AST) for expressions comes in a couple formats.
The first is the fully general version and is labeled with a prime (Expr').
The others are specialized versions of the AST that represent specific phases
of the compilation process. I expect there to be more phases as we begin to
enrich the AST with more information.
-}
module AST.Expression.General where

import Text.PrettyPrint as P

import qualified AST.Helpers as Help
import qualified AST.Literal as Literal
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


---- GENERAL AST ----

type Def =
  A.Located Def'


data Def'
    = Definition Pattern.RawPattern Expr
    | TypeAnnotation String Type.Raw
    deriving (Show)


type Expr =
    A.Annotated R.Region Expr'


data Expr'
    = Literal Literal.Literal
    | Var Var.Raw
    | Range Expr Expr
    | ExplicitList [Expr]
    | Binop Var.Raw Expr Expr
    | Lambda (Pattern.Pattern R.Region Var.Raw) Expr
    | App Expr Expr
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case Expr [(Pattern.Pattern R.Region Var.Raw, Expr)]
    | Data String [Expr]
    | Access Expr String
    | Update Expr [(String, Expr)]
    | Record [(String, Expr)]
    -- for type checking and code gen only
    | Port PortImpl
    | GLShader String String Literal.GLShaderTipe
    deriving (Show)


-- PORTS

data PortImpl
    = In String (Type.Port Type.Raw)
    | Out String Expr (Type.Port Type.Raw)
    | Task String Expr (Type.Port Type.Raw)
    deriving (Show)


portName :: PortImpl -> String
portName impl =
  case impl of
    In name _ -> name
    Out name _ _ -> name
    Task name _ _ -> name


---- UTILITIES ----

rawVar :: String -> Expr'
rawVar x =
  Var (Var.Raw x)


tuple :: [Expr] -> Expr'
tuple expressions =
  Data ("_Tuple" ++ show (length expressions)) expressions


saveEnvName :: String
saveEnvName =
  "_save_the_environment!!!"
