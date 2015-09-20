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

{-| This is a fully general Abstract Syntax Tree (AST) for expressions. It has
"type holes" that allow us to enrich the AST with additional information as we
move through the compilation process. The type holes are used to represent:

  def: Definition style. The source syntax separates type annotations and
       definitions, but after parsing we check that they are well formed and
       collapse them.

  var: Representation of variables. Starts as strings, but is later enriched
       with information about what module a variable came from.

-}
type Expr definition variable =
    A.Annotated R.Region (Expr' definition variable)


data Expr' def var
    = Literal Literal.Literal
    | Var var
    | Range (Expr def var) (Expr def var)
    | ExplicitList [Expr def var]
    | Binop var (Expr def var) (Expr def var)
    | Lambda (Pattern.Pattern R.Region var) (Expr def var)
    | App (Expr def var) (Expr def var)
    | If [(Expr def var, Expr def var)] (Expr def var)
    | Let [def] (Expr def var)
    | Case (Expr def var) [(Pattern.Pattern R.Region var, Expr def var)]
    | Data String [Expr def var]
    | Access (Expr def var) String
    | Update (Expr def var) [(String, Expr def var)]
    | Record [(String, Expr def var)]
    -- for type checking and code gen only
    | Port (PortImpl (Expr def var))
    | GLShader String String Literal.GLShaderTipe
    deriving (Show)


-- PORTS

data PortImpl expr
    = In String (Type.Port Type.Raw)
    | Out String expr (Type.Port Type.Raw)
    | Task String expr (Type.Port Type.Raw)
    deriving (Show)


portName :: PortImpl expr -> String
portName impl =
  case impl of
    In name _ -> name
    Out name _ _ -> name
    Task name _ _ -> name


---- UTILITIES ----

rawVar :: String -> Expr' def Var.Raw
rawVar x =
  Var (Var.Raw x)


tuple :: [Expr def var] -> Expr' def var
tuple expressions =
  Data ("_Tuple" ++ show (length expressions)) expressions


saveEnvName :: String
saveEnvName =
  "_save_the_environment!!!"


-- PRETTY PRINTING

instance (P.Pretty def, P.Pretty var, Var.ToString var) => P.Pretty (Expr' def var) where
  pretty dealiaser needsParens expression =
    case expression of
      Literal literal ->
          P.pretty dealiaser needsParens literal

      Var x ->
          P.pretty dealiaser needsParens x

      Range lowExpr highExpr ->
          P.brackets $
              P.pretty dealiaser False lowExpr
              <> P.text ".."
              <> P.pretty dealiaser False highExpr

      ExplicitList elements ->
          P.brackets (P.commaCat (map (P.pretty dealiaser False) elements))

      Binop op (A.A _ (Literal (Literal.IntNum 0))) expr
          | Var.toString op == "-" ->
              P.text "-" <> P.pretty dealiaser True expr

      Binop op leftExpr rightExpr ->
          P.parensIf needsParens $
              P.hang
                  (P.pretty dealiaser True leftExpr)
                  2
                  (P.text op'' <+> P.pretty dealiaser True rightExpr)
        where
          op' = Var.toString op
          op'' = if Help.isOp op' then op' else "`" ++ op' ++ "`"

      Lambda pattern expr ->
          P.parensIf needsParens $
              P.text "\\" <> args <+> P.text "->" <+> P.pretty dealiaser False body
        where
          (patterns, body) =
              collectLambdas expr

          args =
              P.sep (map (P.pretty dealiaser True) (pattern : patterns))

      App expr arg ->
          P.parensIf needsParens $
              P.hang func 2 (P.sep args)
        where
          func:args =
              map (P.pretty dealiaser True) (collectApps expr ++ [arg])

      If branches finally ->
          let
            prettyFinally =
              P.nest 4 (P.pretty dealiaser False finally)

            prettyBranch (condition, thenBranch) (firstLine, rest) =
              ( P.text "if" <+> P.pretty dealiaser False condition <+> P.text "then"
              ,
                [ P.nest 4 (P.pretty dealiaser False thenBranch)
                , P.text "else" <+> firstLine
                ]
                ++ rest
              )
          in
            P.parensIf needsParens $ P.sep $ uncurry (:) $
                foldr
                    prettyBranch
                    ( P.empty, [prettyFinally] )
                    branches

      Let defs body ->
          P.parensIf needsParens $
              P.sep
                [ P.hang
                    (P.text "let")
                    4
                    (P.vcat (map (P.pretty dealiaser False) defs))
                , P.text "in" <+> P.pretty dealiaser False body
                ]

      Case expr branches ->
          P.parensIf needsParens $
              P.hang pexpr 2 (P.vcat (map pretty' branches))
        where
          pexpr =
              P.text "case" <+> P.pretty dealiaser False expr <+> P.text "of"

          pretty' (pattern, branch) =
              P.pretty dealiaser False pattern
              <+> P.text "->"
              <+> P.pretty dealiaser False branch

      Data "::" [hd,tl] ->
          P.parensIf needsParens $
              P.pretty dealiaser True hd <+> P.text "::" <+> P.pretty dealiaser True tl

      Data "[]" [] ->
          P.text "[]"

      Data name exprs ->
          if Help.isTuple name
            then
              P.parens (P.commaCat (map (P.pretty dealiaser False) exprs))
            else
              P.parensIf (needsParens && not (null exprs)) $
                  P.hang
                      (P.text name)
                      2
                      (P.sep (map (P.pretty dealiaser True) exprs))

      Access record field ->
          P.pretty dealiaser True record <> P.text "." <> P.text field

      Update record fields ->
          P.braces $
              P.hang
                  (P.pretty dealiaser False record <+> P.text "|")
                  4
                  (P.commaSep $ map prettyField fields)
        where
          prettyField (field, expr) =
              P.text field <+> P.text "<-" <+> P.pretty dealiaser False expr

      Record fields ->
          P.sep
            [ P.cat (zipWith (<+>) (P.lbrace : repeat P.comma) (map field fields))
            , P.rbrace
            ]
        where
          field (name, expr) =
             P.text name <+> P.equals <+> P.pretty dealiaser False expr

      GLShader _ _ _ ->
          P.text "[glsl| ... |]"

      Port portImpl ->
          P.pretty dealiaser needsParens portImpl


instance P.Pretty (PortImpl expr) where
  pretty _ _ impl =
      P.text ("<port:" ++ portName impl ++ ">")


collectApps :: Expr def var -> [Expr def var]
collectApps annExpr@(A.A _ expr) =
  case expr of
    App a b -> collectApps a ++ [b]
    _ -> [annExpr]


collectLambdas
    :: Expr def var
    -> ([Pattern.Pattern R.Region var], Expr def var)
collectLambdas lexpr@(A.A _ expr) =
  case expr of
    Lambda pattern body ->
        let
          (ps, body') = collectLambdas body
        in
          (pattern : ps, body')

    _ -> ([], lexpr)
