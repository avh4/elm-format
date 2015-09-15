{-# OPTIONS_GHC -Wall #-}
module Format where

import Elm.Utils ((|>))

import qualified AST.Declaration as D
import qualified AST.Expression.General as EG
import qualified AST.Expression.Source as E
import qualified AST.Literal as L
import qualified AST.Module as M
import qualified AST.Module.Name as MN
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as V
import qualified Data.Map as Map
import qualified Reporting.Annotation as RA
import qualified Data.List as List


(>-) = flip (.)


foldw tx write list =
    flip (foldl $ flip $ tx write) list


formatModule :: (String -> a -> a) -> M.SourceModule -> a -> a
formatModule write mod =
    write "module "
    >- write (formatName $ M.name mod)
    >- write " where\n\n"
    >- write imports
    >- foldw formatDeclaration write (M.body mod)
    where
        imports =
            case M.imports mod of
                [] ->
                    ""
                _ ->
                    (M.imports mod |> map formatImport |> List.intercalate "\n") ++ "\n\n"


formatName :: MN.Canonical -> String
formatName name = formatRawName $ MN._module name


formatRawName :: MN.Raw -> String
formatRawName name = List.intercalate "." name


formatImport :: M.UserImport -> String
formatImport aimport =
    case RA.drop aimport of
        (name,method) ->
            "import " ++ (formatRawName name) ++ as ++ exposing
            where
                as =
                    if (M.alias method) == (Just $ formatRawName name)
                        then ""
                    else
                        case M.alias method of
                            Nothing -> "<nothing>"
                            Just name -> " as " ++ name
                exposing =
                    case M.exposedVars method of
                        V.Listing [] False -> ""
                        V.Listing [] True -> " exposing (..)"
                        V.Listing vars False ->
                            " exposing (" ++ (vars |> map formatVarValue |> List.intercalate ", " ) ++ ")"
                        V.Listing _ True -> "<NOT POSSIBLE?>"


formatVarValue :: V.Value -> String
formatVarValue aval =
    case aval of
        V.Value val -> val
        V.Alias _ -> "<alias>"
        V.Union _ _ -> "<union>"


formatDeclaration :: (String -> a -> a) -> D.SourceDecl -> a -> a
formatDeclaration write decl =
    case decl of
        D.Comment s -> write "<comment>"
        D.Decl adecl ->
            case RA.drop adecl of
                D.Definition def -> formatDefinition write def
                D.Datatype _ _ _ -> write "<datatype>"
                D.TypeAlias _ _ _ -> write "<typealias>"
                D.Port port -> write "<port>"
                D.Fixity _ _ _ -> write "<fixity>"


formatDefinition :: (String -> a -> a) -> E.Def -> a -> a
formatDefinition write adef =
    case RA.drop adef of
        E.Definition pattern expr ->
            formatPattern write pattern
            >- write " =\n    "
            >- formatExpression write expr
            >- write "\n"
        E.TypeAnnotation name typ ->
            write name
            >- write " : "
            >- formatType write typ
            >- write "\n"


formatPattern :: (String -> a -> a) -> P.RawPattern -> a -> a
formatPattern write apattern =
    case RA.drop apattern of
        P.Data _ _ -> write "<data>"
        P.Record _ -> write "<record>"
        P.Alias _ _ -> write "<alias>"
        P.Var var -> write var
        P.Anything -> write "<anything>"
        P.Literal _ -> write "<literal>"


formatExpression :: (String -> a -> a) -> E.Expr -> a -> a
formatExpression write aexpr =
    case RA.drop aexpr of
        EG.Literal lit -> formatLiteral write lit
        EG.Var _ -> write "<var>"
        EG.Range _ _ -> write "<range>"
        EG.ExplicitList _ -> write "<list>"
        EG.Binop _ _ _ -> write "<binop>"
        EG.Lambda _ _ -> write "<lambda>"
        EG.App _ _ -> write "<app>"
        EG.If _ _ -> write "<if>"
        EG.Let _ _ -> write "<let>"
        EG.Case _ _ -> write "<case>"
        EG.Data _ _ -> write "<data>"
        EG.Access _ _ -> write "<access>"
        EG.Update _ _ -> write "<update>"
        EG.Record _ -> write "<record>"
        EG.Port _ -> write "<port>"
        EG.GLShader _ _ _ -> write "<glshader>"


formatLiteral :: (String -> a -> a) -> L.Literal -> a -> a
formatLiteral write lit =
    case lit of
        L.IntNum _ -> write "<int>"
        L.FloatNum _ -> write "<float>"
        L.Chr _ -> write "<char>"
        L.Str s -> write $ "\"" ++ s ++ "\"" -- TODO: quoting
        L.Boolean _ -> write "<boolean>"


formatType :: (String -> a -> a) -> T.Raw -> a -> a
formatType write atype =
    case RA.drop atype of
        T.RLambda _ _ -> write "<lambda>"
        T.RVar var -> write var -- TODO: not tested
        T.RType var -> formatVar write var
        T.RApp _ _ -> write "<app>"
        T.RRecord _ _ -> write "<record>"


formatVar :: (String -> a -> a) -> V.Raw -> a -> a
formatVar write (V.Raw var) =
    write var
