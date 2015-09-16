{-# OPTIONS_GHC -Wall #-}
module Format where

import Elm.Utils ((|>))
import Text.PrettyPrint.Boxes (Box, (<>), (//))

import qualified AST.Declaration as D
import qualified AST.Expression.General as EG
import qualified AST.Expression.Source as E
import qualified AST.Literal as L
import qualified AST.Module as M
import qualified AST.Module.Name as MN
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as V
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Reporting.Annotation as RA
import qualified Text.PrettyPrint.Boxes as Box


indent :: Box -> Box
indent box =
    Box.text "    " <> box


formatModule :: M.SourceModule -> Box
formatModule mod =
    Box.vcat Box.top
        [ Box.hcat Box.left
            [ Box.text "module "
            , formatName $ M.name mod
            , Box.text " where"
            ]
        , Box.emptyBox 1 0
        , case M.imports mod of
            [] ->
                Box.nullBox
            imports ->
                Box.vcat Box.top (map formatImport imports)
                // Box.emptyBox 1 0
        , Box.vcat Box.top (map formatDeclaration $ M.body mod)
        ]


formatName :: MN.Canonical -> Box
formatName name = formatRawName $ MN._module name


formatRawName :: MN.Raw -> Box
formatRawName name =
    Box.punctuateH Box.left (Box.char '.') (map Box.text name)


formatImport :: M.UserImport -> Box
formatImport aimport =
    case RA.drop aimport of
        (name,method) ->
            Box.hcat Box.left
                [ Box.text "import "
                , formatRawName name
                , as
                , exposing
                ]
            where
                as =
                    if (M.alias method) == (Just $ List.intercalate "." name)
                        then Box.nullBox
                    else
                        case M.alias method of
                            Nothing -> Box.text "<nothing>"
                            Just name -> Box.text $ " as " ++ name
                exposing =
                    case M.exposedVars method of
                        V.Listing [] False -> Box.nullBox
                        V.Listing [] True -> Box.text " exposing (..)"
                        V.Listing vars False ->
                            Box.hcat Box.left
                                [ Box.text " exposing ("
                                , Box.punctuateH Box.left (Box.text ", ") (map formatVarValue vars)
                                , Box.text ")"
                                ]
                        V.Listing _ True -> Box.text "<NOT POSSIBLE?>"


formatVarValue :: V.Value -> Box
formatVarValue aval =
    case aval of
        V.Value val -> Box.text val
        V.Alias _ -> Box.text "<alias>"
        V.Union _ _ -> Box.text "<union>"


formatDeclaration :: D.SourceDecl -> Box
formatDeclaration decl =
    case decl of
        D.Comment s -> Box.text "<comment>"
        D.Decl adecl ->
            case RA.drop adecl of
                D.Definition def -> formatDefinition def
                D.Datatype _ _ _ -> Box.text "<datatype>"
                D.TypeAlias _ _ _ -> Box.text "<typealias>"
                D.Port port -> Box.text "<port>"
                D.Fixity _ _ _ -> Box.text "<fixity>"


formatDefinition :: E.Def -> Box
formatDefinition adef =
    case RA.drop adef of
        E.Definition pattern expr ->
            Box.vcat Box.top
                [ Box.hcat Box.left
                    [ formatPattern pattern
                    , Box.text " ="
                    ]
                , indent $ formatExpression expr
                ]
        E.TypeAnnotation name typ ->
            Box.hcat Box.left
                [ Box.text name
                , Box.text " : "
                , formatType typ
                ]


formatPattern :: P.RawPattern -> Box
formatPattern apattern =
    case RA.drop apattern of
        P.Data _ _ -> Box.text "<data>"
        P.Record _ -> Box.text "<record>"
        P.Alias _ _ -> Box.text "<alias>"
        P.Var var -> Box.text var
        P.Anything -> Box.text "<anything>"
        P.Literal _ -> Box.text "<literal>"


formatExpression :: E.Expr -> Box
formatExpression aexpr =
    case RA.drop aexpr of
        EG.Literal lit -> formatLiteral lit
        EG.Var _ -> Box.text "<var>"
        EG.Range _ _ -> Box.text "<range>"
        EG.ExplicitList _ -> Box.text "<list>"
        EG.Binop _ _ _ -> Box.text "<binop>"
        EG.Lambda _ _ -> Box.text "<lambda>"
        EG.App _ _ -> Box.text "<app>"
        EG.If _ _ -> Box.text "<if>"
        EG.Let _ _ -> Box.text "<let>"
        EG.Case _ _ -> Box.text "<case>"
        EG.Data _ _ -> Box.text "<data>"
        EG.Access _ _ -> Box.text "<access>"
        EG.Update _ _ -> Box.text "<update>"
        EG.Record _ -> Box.text "<record>"
        EG.Port _ -> Box.text "<port>"
        EG.GLShader _ _ _ -> Box.text "<glshader>"


formatLiteral :: L.Literal -> Box
formatLiteral lit =
    case lit of
        L.IntNum _ -> Box.text "<int>"
        L.FloatNum _ -> Box.text "<float>"
        L.Chr _ -> Box.text "<char>"
        L.Str s -> Box.text $ "\"" ++ s ++ "\"" -- TODO: quoting
        L.Boolean _ -> Box.text "<boolean>"


formatType :: T.Raw -> Box
formatType atype =
    case RA.drop atype of
        T.RLambda _ _ -> Box.text "<lambda>"
        T.RVar var -> Box.text var -- TODO: not tested
        T.RType var -> formatVar var
        T.RApp _ _ -> Box.text "<app>"
        T.RRecord _ _ -> Box.text "<record>"


formatVar :: V.Raw -> Box
formatVar (V.Raw var) =
    Box.text var
