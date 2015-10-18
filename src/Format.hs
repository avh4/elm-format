{-# OPTIONS_GHC -Wall #-}
module Format where

import Elm.Utils ((|>))
import Box

import AST.V0_15
import qualified AST.Declaration
import qualified AST.Expression
import qualified AST.Literal as L
import qualified AST.Module
import qualified AST.Module.Name as MN
import qualified AST.Pattern
import qualified AST.Type
import qualified AST.Variable
import qualified Data.List as List
import qualified Reporting.Annotation as RA


formatModule :: AST.Module.Module -> Box
formatModule modu =
    vbox
        [ hbox
            [ text "module "
            , formatName $ AST.Module.name modu
            , formatListing $ AST.Module.exports modu
            , text " where"
            ]
            |> margin 1
            , formatModuleDocs $ AST.Module.docs modu
        , case AST.Module.imports modu of
            [] ->
                empty
            imports ->
                vbox (map formatImport imports)
                |> margin 2
        , vbox (map formatDeclaration $ AST.Module.body modu)
        ]


formatModuleDocs :: RA.Located (Maybe String) -> Box
formatModuleDocs adocs =
    case RA.drop adocs of
        Nothing ->
            empty
        Just docs ->
            formatDocComment docs
            |> margin 1


formatDocComment :: String -> Box
formatDocComment docs =
    hbox
        [ text "{-| "
        , text $ docs
        , text "-}"
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
                    case AST.Module.alias method of
                        Nothing ->
                            empty
                        Just alias ->
                            text $ " as " ++ alias
                exposing =
                    case AST.Module.exposedVars method of
                        AST.Variable.Listing [] False ->
                            empty
                        AST.Variable.Listing [] True ->
                            text " exposing (..)"
                        AST.Variable.Listing vars False ->
                            hbox
                                [ text " exposing ("
                                , hjoin (text ", ") (map formatVarValue vars)
                                , text ")"
                                ]
                        AST.Variable.Listing _ True ->
                            text "<NOT POSSIBLE?>"


formatListing :: AST.Variable.Listing (RA.Located AST.Variable.Value)-> Box
formatListing listing =
    case listing of
        AST.Variable.Listing [] False ->
            empty
        AST.Variable.Listing [] True ->
            text " (..)"
        AST.Variable.Listing vars False ->
            hbox
                [ text " ("
                , hjoin (text ", ") (map (formatVarValue . RA.drop) vars)
                , text ")"
                ]
        AST.Variable.Listing _ True ->
            text "<NOT POSSIBLE?>"


formatStringListing :: AST.Variable.Listing String -> Box
formatStringListing listing =
    case listing of
        AST.Variable.Listing [] False ->
            empty
        AST.Variable.Listing [] True ->
            text "(..)"
        AST.Variable.Listing vars False ->
            hbox
                [ text "("
                , hjoin (text ",") (map text vars)
                , text ")"
                ]
        AST.Variable.Listing _ True ->
            text "<NOT POSSIBLE?>"


formatVarValue :: AST.Variable.Value -> Box
formatVarValue aval =
    case aval of
        AST.Variable.Value val ->
            formatCommented formatVar val -- TODO: comments not tested
        AST.Variable.Alias name ->
            text name
        AST.Variable.Union name listing ->
            hbox
                [ text name
                , formatStringListing listing
                ]


formatDeclaration :: AST.Declaration.Decl -> Box
formatDeclaration decl =
    case decl of
        AST.Declaration.Comment docs ->
            formatDocComment docs
        AST.Declaration.Decl adecl ->
            case RA.drop adecl of
                AST.Declaration.Definition def ->
                    formatDefinition def
                AST.Declaration.Datatype _ _ _ -> text "<datatype>"
                AST.Declaration.TypeAlias name args typ ->
                    vbox
                        [ hboxlist "type alias " " " " =" text (name:args)
                        , formatType typ
                            |> indent
                        ]
                    |> margin 2
                AST.Declaration.PortAnnotation _ _ -> text "<port annotation>"
                AST.Declaration.PortDefinition _ _ -> text "<port definition>"
                AST.Declaration.Fixity _ _ _ -> text "<fixity>"


formatDefinition :: AST.Expression.Def -> Box
formatDefinition adef =
    case RA.drop adef of
        AST.Expression.Definition name args expr ->
            vbox
                [ hbox
                    [ formatPattern name
                    , hbox $ List.map (\arg -> hbox [ text " ", formatPattern arg]) args
                    , text " ="
                    ]
                , formatExpression expr
                    |> indent
                    |> margin 2
                ]
        AST.Expression.TypeAnnotation var typ ->
            hbox
                [ formatCommented formatVar var -- TODO: comments not tested
                , text " : "
                , formatType typ
                ]


formatPattern :: AST.Pattern.Pattern -> Box
formatPattern apattern =
    case RA.drop apattern of
        AST.Pattern.Data _ _ -> text "<pattern data>"
        AST.Pattern.Tuple patterns ->
            hboxlist "(" "," ")" formatPattern patterns
        AST.Pattern.Record fields ->
            hboxlist "{" "," "}" text fields
        AST.Pattern.Alias _ _ -> text "<alias>"
        AST.Pattern.Var var ->
            formatCommented formatVar var -- TODO: comments not tested
        AST.Pattern.Anything ->
            text "_"
        AST.Pattern.Literal _ -> text "<literal>"


formatExpression :: AST.Expression.Expr -> Box
formatExpression aexpr =
    case RA.drop aexpr of
        AST.Expression.Literal lit ->
            formatCommented formatLiteral lit
        AST.Expression.Var v ->
            formatCommented formatVar v -- TODO: comments not tested
        AST.Expression.Range _ _ -> text "<range>"
        AST.Expression.ExplicitList _ -> text "<list>"
        AST.Expression.Binops l ops ->
            let opBoxes (op,e) =
                  [ hspace 1, formatCommented formatInfixVar op
                  , hspace 1, formatExpression e
                  ]
            in
                hbox $
                    formatExpression l
                    : concatMap opBoxes ops
        AST.Expression.Lambda patterns expr ->
            hbox
                [ hboxlist "\\" " " " -> " formatPattern patterns
                , formatExpression expr
                ]
        AST.Expression.Unary AST.Expression.Negative e ->
            hbox
                [ text "-"
                , formatExpression e
                ]
        AST.Expression.App l r ->
            hbox
                [ formatExpression l
                , hspace 1
                , formatExpression r
                ]
        AST.Expression.If _ _ -> text "<if>"
        AST.Expression.Let _ _ -> text "<let>"
        AST.Expression.Case _ _ -> text "<case>"
        AST.Expression.Data _ _ -> text "<expression data>"
        AST.Expression.Tuple exprs ->
            hboxlist "(" ", " ")" formatExpression exprs
        AST.Expression.Access _ _ -> text "<access>"
        AST.Expression.Update _ _ -> text "<update>"
        AST.Expression.Record _ -> text "<record>"
        AST.Expression.Parens expr ->
            hbox
                [ text "("
                , formatExpression expr
                , text ")"
                ]
        AST.Expression.Port _ -> text "<port>"
        AST.Expression.GLShader _ _ _ -> text "<glshader>"


formatCommented :: (a -> Box) -> Commented a -> Box
formatCommented format commented =
    case commented of
        Commented comments inner ->
            hbox
                [ hbox (map formatComment comments)
                , format inner
                ]


formatComment :: String -> Box
formatComment comment =
    text $ "{- " ++ comment ++ " -} "


formatLiteral :: L.Literal -> Box
formatLiteral lit =
    case lit of
        L.IntNum i ->
            text $ show i
        L.FloatNum f ->
            text $ show f
        L.Chr c ->
            text ['\'', c, '\''] -- TODO: escape specials
        L.Str s ->
            text $ "\"" ++ s ++ "\"" -- TODO: escaping
        L.Boolean True ->
            text "True"
        L.Boolean False ->
            text "False" -- TODO: not tested


formatType :: AST.Type.Type -> Box
formatType atype =
    case RA.drop atype of
        AST.Type.RLambda left right ->
            hbox
                [ text "("
                , formatType left
                , text " -> "
                , formatType right
                , text ")"
                ]
        AST.Type.RVar var ->
            text var
        AST.Type.RType var ->
            formatVar var
        AST.Type.RApp ctor args ->
            hboxlist "" " " "" formatType (ctor:args)
        AST.Type.RTuple args ->
            hboxlist "(" ", " ")" formatType args
        AST.Type.RRecord ext fields multiline ->
            let
                start =
                    case ext of
                        Nothing ->
                            text "{ "
                        Just base ->
                            hbox
                                [ text "{ "
                                , formatType base
                                , text " | "
                                ]
                (vStart,vPrefix) =
                    case ext of
                        Nothing ->
                            (empty, "{ ")
                        Just base ->
                            (hbox2 (text "{ ") (formatType base), "| ")
                pair (k,v) =
                    hbox
                        [ text k
                        , text " : "
                        , formatType v
                        ]
            in
                case multiline of
                    False ->
                        hbox2 start $ hboxlist "" ", " " }" pair fields
                    True ->
                        vbox2 vStart $ vboxlist vPrefix ", " "}" pair fields


formatVar :: AST.Variable.Ref -> Box
formatVar var =
    case var of
        AST.Variable.VarRef name ->
            text name
        AST.Variable.OpRef name ->
            text $ "(" ++ name ++ ")"
        AST.Variable.WildcardRef ->
            text "_" -- TODO: not tested


formatInfixVar :: AST.Variable.Ref -> Box
formatInfixVar var =
    case var of
        AST.Variable.VarRef name ->
            text $ "`" ++ name ++ "`" -- TODO: not tested
        AST.Variable.OpRef name ->
            text name
        AST.Variable.WildcardRef ->
            text "_" -- TODO: should never happen
