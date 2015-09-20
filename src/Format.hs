{-# OPTIONS_GHC -Wall #-}
module Format where

import Elm.Utils ((|>))
import Box

import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Literal as L
import qualified AST.Module
import qualified AST.Module.Name as MN
import qualified AST.Pattern as P
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


formatDeclaration :: D.SourceDecl -> Box
formatDeclaration decl =
    case decl of
        D.Comment s -> text "<comment>"
        D.Decl adecl ->
            case RA.drop adecl of
                D.Definition def -> formatDefinition def
                D.Datatype _ _ _ -> text "<datatype>"
                D.TypeAlias _ _ _ -> text "<typealias>"
                D.Port port -> text "<port>"
                D.Fixity _ _ _ -> text "<fixity>"


formatDefinition :: E.Def -> Box
formatDefinition adef =
    case RA.drop adef of
        E.Definition pattern expr ->
            vbox
                [ hbox
                    [ formatPattern pattern
                    , text " ="
                    ]
                , formatExpression expr
                    |> indent
                    |> margin 2
                ]
        E.TypeAnnotation name typ ->
            hbox
                [ text name
                , text " : "
                , formatType typ
                ]


formatPattern :: P.RawPattern -> Box
formatPattern apattern =
    case RA.drop apattern of
        P.Data _ _ -> text "<data>"
        P.Record _ -> text "<record>"
        P.Alias _ _ -> text "<alias>"
        P.Var var -> text var
        P.Anything -> text "<anything>"
        P.Literal _ -> text "<literal>"


formatExpression :: E.Expr -> Box
formatExpression aexpr =
    case RA.drop aexpr of
        E.Literal lit -> formatLiteral lit
        E.Var _ -> text "<var>"
        E.Range _ _ -> text "<range>"
        E.ExplicitList _ -> text "<list>"
        E.Binop _ _ _ -> text "<binop>"
        E.Lambda _ _ -> text "<lambda expression>"
        E.App _ _ -> text "<app>"
        E.If _ _ -> text "<if>"
        E.Let _ _ -> text "<let>"
        E.Case _ _ -> text "<case>"
        E.Data _ _ -> text "<data>"
        E.Access _ _ -> text "<access>"
        E.Update _ _ -> text "<update>"
        E.Record _ -> text "<record>"
        E.Port _ -> text "<port>"
        E.GLShader _ _ _ -> text "<glshader>"


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


formatVar :: V.Raw -> Box
formatVar (V.Raw var) =
    text var
