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
        , formatModuleDocs (AST.Module.docs modu)
        , case AST.Module.imports modu of
            [] ->
                empty
            imports ->
                imports
                |> List.sortOn (fst . RA.drop)
                |> map formatImport
                |> vbox
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
                    formatDefinition False def
                AST.Declaration.Datatype _ _ _ -> text "<datatype>"
                AST.Declaration.TypeAlias name args typ ->
                    vbox
                        [ hboxlist "type alias " " " " =" text (name:args)
                        , formatType typ
                            |> indent 4
                        ]
                    |> margin 2
                AST.Declaration.PortAnnotation _ _ -> text "<port annotation>"
                AST.Declaration.PortDefinition _ _ -> text "<port definition>"
                AST.Declaration.Fixity _ _ _ -> text "<fixity>"


formatDefinition :: Bool -> AST.Expression.Def -> Box
formatDefinition compact adef =
    case RA.drop adef of
        AST.Expression.Definition name args expr multiline ->
            case compact && (not multiline) of
                False ->
                    vbox
                        [ hbox
                            [ formatPattern name
                            , hbox $ List.map (\arg -> hbox [ text " ", formatPattern arg]) args
                            , text " ="
                            ]
                        , formatExpression False expr
                            |> indent 4
                        ]
                    |> margin (if compact then 1 else 2)
                True ->
                    hbox
                        [ formatPattern name
                        , hbox $ List.map (\arg -> hbox [ text " ", formatPattern arg]) args
                        , text " = "
                        , formatExpression False expr
                        ]
                    |> margin 1
        AST.Expression.TypeAnnotation var typ ->
            hbox
                [ formatCommented formatVar var -- TODO: comments not tested
                , text " : "
                , formatType typ
                ]


formatPattern :: AST.Pattern.Pattern -> Box
formatPattern apattern =
    case RA.drop apattern of
        AST.Pattern.Data ctor patterns ->
            hbox2
                (formatVar ctor)
                (hboxlist (if List.null patterns then "" else " ") " " "" formatPattern patterns)
        AST.Pattern.Tuple patterns ->
            hboxlist "(" ", " ")" formatPattern patterns
        AST.Pattern.Record fields ->
            hboxlist "{" ", " "}" text fields
        AST.Pattern.Alias _ _ -> text "<alias>"
        AST.Pattern.Var var ->
            formatCommented formatVar var -- TODO: comments not tested
        AST.Pattern.Anything ->
            text "_"
        AST.Pattern.Literal lit ->
            formatLiteral lit


formatExpression :: Bool -> AST.Expression.Expr -> Box
formatExpression inList aexpr =
    case RA.drop aexpr of
        AST.Expression.Literal lit ->
            formatCommented formatLiteral lit
        AST.Expression.Var v ->
            formatCommented formatVar v -- TODO: comments not tested
        AST.Expression.Range _ _ -> text "<range>"
        AST.Expression.ExplicitList exprs False ->
            case exprs of
                [] ->
                    text "[]"
                _ ->
                    hboxlist "[ " ", " " ]" (formatExpression False) exprs
        AST.Expression.ExplicitList exprs True ->
            case exprs of
                [] ->
                    text "[]"
                _ ->
                    vboxlist "[ " ", " "]" (formatExpression True) exprs

        AST.Expression.Binops l ops False ->
            let
                opBoxes (op,e) =
                  [ hspace 1
                  , formatCommented formatInfixVar op
                  , hspace 1
                  , formatExpression False e
                  ]
            in
                hbox $
                    formatExpression False l
                    : concatMap opBoxes ops
        AST.Expression.Binops l ops True ->
            let
                formatOp (op,e) =
                    hbox
                        [ formatCommented formatInfixVar op -- TODO: comments not tested
                        , hspace 1
                        , formatExpression False e
                        ]
            in
                vbox
                    [ formatExpression False l
                    , vbox (map formatOp ops)
                        |> indent 4
                    ]

        AST.Expression.Lambda patterns expr ->
            hbox
                [ hboxlist "\\" " " " -> " formatPattern patterns
                , formatExpression False expr
                ]
        AST.Expression.Unary AST.Expression.Negative e ->
            hbox
                [ text "-"
                , formatExpression False e
                ]
        AST.Expression.App l rs multiline ->
            case multiline of
                False ->
                    hbox
                        [ formatExpression False l
                        , hboxlist " " " " "" (formatExpression False) rs
                        ]
                True ->
                    vbox2
                        (formatExpression False l)
                        (vboxlist "" "" "" (formatExpression False) rs
                            |> indent (if inList then 2 else 4))

        AST.Expression.If [] els ->
            vbox
                [ text "<INVALID IF EXPRESSION>"
                , formatExpression False els
                    |> indent 4
                ]
        AST.Expression.If ((if0,body0):elseifs) els ->
            vbox
                [ hbox
                    [ text "if "
                    , formatExpression False if0
                    , text " then"
                    ]
                , formatExpression False body0
                    |> indent 4
                , let
                    formatElseIf (if',body') =
                        vbox
                            [ hbox
                                [ text "else if "
                                , formatExpression False if'
                                , text " then"
                                ]
                            , formatExpression False body'
                                |> indent 4
                            ]
                  in
                      vbox (map formatElseIf elseifs)
                , text "else"
                , formatExpression False els
                    |> indent 4
                ]

        AST.Expression.Let defs expr ->
            vbox
                [ text "let"
                , vboxlist "" "" "" (formatDefinition True) defs
                    |> indent 4
                    |> margin 0
                , text "in"
                , formatExpression False expr
                    |> indent 4
                ]
        AST.Expression.Case subject clauses ->
            vbox
                [ hbox
                    [ text "case "
                    , formatExpression False subject
                    , text " of"
                    ]
                , let
                      formatClause (pat,expr) =
                          vbox
                              [ hbox
                                  [ formatPattern pat
                                  , text " ->"
                                  ]
                              , formatExpression False expr
                                  |> indent 4
                              ]
                  in
                      vbox (map formatClause clauses)
                      |> indent 4
                ]
        AST.Expression.Data _ _ -> text "<expression data>"
        AST.Expression.Tuple exprs ->
            hboxlist "(" ", " ")" (formatExpression False) exprs

        AST.Expression.Access expr field ->
            hbox
                [ formatExpression False expr -- TODO: needs to have parens in some cases
                , text "."
                , text field
                ]

        AST.Expression.AccessFunction field ->
            hbox2 (text ".") (text field)

        AST.Expression.Update _ _ -> text "<update>"
        AST.Expression.Record pairs -> -- TODO: single-line records
            let
                vStart = empty
                vPrefix = "{ "
                pair (k,v) =
                    hbox
                        [ text k
                        , text " = "
                        , formatExpression False v
                        ]
            in
                vbox2 vStart $ vboxlist vPrefix ", " "}" pair pairs
        AST.Expression.Parens expr ->
            hbox
                [ text "("
                , formatExpression False expr
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


data TypeParensRequired
    = ForLambda
    | ForCtor
    | NotRequired
    deriving (Eq)


formatType :: AST.Type.Type -> Box
formatType =
    formatType' NotRequired


formatType' :: TypeParensRequired -> AST.Type.Type -> Box
formatType' requireParens atype =
    case RA.drop atype of
        AST.Type.RLambda left right ->
            hbox
                [ if requireParens /= NotRequired then text "(" else empty
                , formatType' ForLambda left
                , text " -> "
                , formatType right
                , if requireParens /= NotRequired then text ")" else empty
                ]
        AST.Type.RVar var ->
            text var
        AST.Type.RType var ->
            formatVar var
        AST.Type.RApp ctor args ->
            hboxlist
                (if requireParens == ForCtor then "(" else "") " "
                (if requireParens == ForCtor then ")" else "")
                (formatType' ForCtor) (ctor:args)
        AST.Type.RTuple args ->
            hboxlist "(" ", " ")" (formatType) args
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
                pair (k,v,multiline') =
                    case multiline' of
                        False ->
                            hbox
                                [ text k
                                , text " : "
                                , formatType v
                                ]
                        True ->
                            vbox2
                                (hbox2 (text k) (text " :"))
                                (formatType v |> indent 2)
            in
                case multiline of
                    False ->
                        case fields of
                            [] ->
                                text "{}"
                            _ ->
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
