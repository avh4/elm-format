{-# OPTIONS_GHC -Wall #-}
module Format where

import Elm.Utils ((|>))
import Box

import qualified AST.Declaration
import qualified AST.Expression
import qualified AST.Literal as L
import qualified AST.Module
import qualified AST.Module.Name as MN
import qualified AST.Pattern
import qualified AST.Type as T
import qualified AST.Variable as V
import qualified Data.List as List
import qualified Reporting.Annotation as RA


formatModule :: AST.Module.Module -> Box
formatModule mod =
    vbox
        [ hbox
            [ text "module "
            , formatName $ AST.Module.name mod
            , text " where"
            ]
            |> margin 1
        , case AST.Module.imports mod of
            [] ->
                empty
            imports ->
                vbox (map formatImport imports)
                |> margin 2
        , vbox (map formatDeclaration $ AST.Module.body mod)
        ]


formatName :: MN.Canonical -> Box
formatName name = formatRawName $ MN._module name


formatRawName :: MN.Raw -> Box
formatRawName name =
    text (List.intercalate "." name)


formatImport :: AST.Module.UserImport -> Box
formatImport aimport =
    case RA.drop aimport of
        (name,method) ->
            hbox
                [ text "import "
                , formatRawName name
                , as
                , exposing
                ]
            where
                as =
                    if (AST.Module.alias method) == (Just $ List.intercalate "." name)
                        then empty
                    else
                        case AST.Module.alias method of
                            Nothing -> text "<nothing>"
                            Just name -> text $ " as " ++ name
                exposing =
                    case AST.Module.exposedVars method of
                        V.Listing [] False -> empty
                        V.Listing [] True -> text " exposing (..)"
                        V.Listing vars False ->
                            hbox
                                [ text " exposing ("
                                , hjoin (text ", ") (map formatVarValue vars)
                                , text ")"
                                ]
                        V.Listing _ True -> text "<NOT POSSIBLE?>"


formatVarValue :: V.Value -> Box
formatVarValue aval =
    case aval of
        V.Value val -> text val
        V.Alias _ -> text "<alias>"
        V.Union _ _ -> text "<union>"


formatDeclaration :: AST.Declaration.Decl -> Box
formatDeclaration decl =
    case decl of
        AST.Declaration.Comment s -> text "<comment>"
        AST.Declaration.Decl adecl ->
            case RA.drop adecl of
                AST.Declaration.Definition def -> formatDefinition def
                AST.Declaration.Datatype _ _ _ -> text "<datatype>"
                AST.Declaration.TypeAlias _ _ _ -> text "<typealias>"
                AST.Declaration.Port port -> text "<port>"
                AST.Declaration.Fixity _ _ _ -> text "<fixity>"


formatDefinition :: AST.Expression.Def -> Box
formatDefinition adef =
    case RA.drop adef of
        AST.Expression.Definition pattern expr ->
            vbox
                [ hbox
                    [ formatPattern pattern
                    , text " ="
                    ]
                , formatExpression expr
                    |> indent
                    |> margin 2
                ]
        AST.Expression.TypeAnnotation name typ ->
            hbox
                [ text name
                , text " : "
                , formatType typ
                ]


formatPattern :: AST.Pattern.Pattern -> Box
formatPattern apattern =
    case RA.drop apattern of
        AST.Pattern.Data _ _ -> text "<data>"
        AST.Pattern.Record _ -> text "<record>"
        AST.Pattern.Alias _ _ -> text "<alias>"
        AST.Pattern.Var var -> text var
        AST.Pattern.Anything -> text "<anything>"
        AST.Pattern.Literal _ -> text "<literal>"


formatExpression :: AST.Expression.Expr -> Box
formatExpression aexpr =
    case RA.drop aexpr of
        AST.Expression.Literal lit -> formatLiteral lit
        AST.Expression.Var _ -> text "<var>"
        AST.Expression.Range _ _ -> text "<range>"
        AST.Expression.ExplicitList _ -> text "<list>"
        AST.Expression.Binop _ _ _ -> text "<binop>"
        AST.Expression.Lambda _ _ -> text "<lambda expression>"
        AST.Expression.App _ _ -> text "<app>"
        AST.Expression.If _ _ -> text "<if>"
        AST.Expression.Let _ _ -> text "<let>"
        AST.Expression.Case _ _ -> text "<case>"
        AST.Expression.Data _ _ -> text "<data>"
        AST.Expression.Access _ _ -> text "<access>"
        AST.Expression.Update _ _ -> text "<update>"
        AST.Expression.Record _ -> text "<record>"
        AST.Expression.Port _ -> text "<port>"
        AST.Expression.GLShader _ _ _ -> text "<glshader>"


formatLiteral :: L.Literal -> Box
formatLiteral lit =
    case lit of
        L.IntNum _ -> text "<int>"
        L.FloatNum _ -> text "<float>"
        L.Chr _ -> text "<char>"
        L.Str s ->
            text $ "\"" ++ s ++ "\"" -- TODO: quoting
        L.Boolean _ -> text "<boolean>"


formatType :: T.Raw -> Box
formatType atype =
    case RA.drop atype of
        T.RLambda _ _ -> text "<lambda type>"
        T.RVar var -> text var -- TODO: not tested
        T.RType var -> formatVar var
        T.RApp _ _ -> text "<app>"
        T.RRecord _ _ -> text "<record>"


formatVar :: V.Var -> Box
formatVar (V.Var var) =
    text var
