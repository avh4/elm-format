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
            write (formatPattern pattern)
            >- write " =\n    "
            >- write (formatExpression expr)
            >- write "\n"
        E.TypeAnnotation name typ ->
            write name
            >- write " : "
            >- write (formatType typ)
            >- write "\n"


formatPattern :: P.RawPattern -> String
formatPattern apattern =
    case RA.drop apattern of
        P.Data _ _ -> "<data>"
        P.Record _ -> "<record>"
        P.Alias _ _ -> "<alias>"
        P.Var var -> var
        P.Anything -> "<anything>"
        P.Literal _ -> "<literal>"


formatExpression :: E.Expr -> String
formatExpression aexpr =
    case RA.drop aexpr of
        EG.Literal lit -> formatLiteral lit
        EG.Var _ -> "<var>"
        EG.Range _ _ -> "<range>"
        EG.ExplicitList _ -> "<list>"
        EG.Binop _ _ _ -> "<binop>"
        EG.Lambda _ _ -> "<lambda>"
        EG.App _ _ -> "<app>"
        EG.If _ _ -> "<if>"
        EG.Let _ _ -> "<let>"
        EG.Case _ _ -> "<case>"
        EG.Data _ _ -> "<data>"
        EG.Access _ _ -> "<access>"
        EG.Update _ _ -> "<update>"
        EG.Record _ -> "<record>"
        EG.Port _ -> "<port>"
        EG.GLShader _ _ _ -> "<glshader>"


formatLiteral :: L.Literal -> String
formatLiteral lit =
    case lit of
        L.IntNum _ -> "<int>"
        L.FloatNum _ -> "<float>"
        L.Chr _ -> "<char>"
        L.Str s -> "\"" ++ s ++ "\"" -- TODO: quoting
        L.Boolean _ -> "<boolean>"


formatType :: T.Raw -> String
formatType atype =
    case RA.drop atype of
        T.RLambda _ _ -> "<lambda>"
        T.RVar var -> var -- TODO: not tested
        T.RType var -> formatVar var
        T.RApp _ _ -> "<app>"
        T.RRecord _ _ -> "<record>"


formatVar :: V.Raw -> String
formatVar (V.Raw var) =
    var
