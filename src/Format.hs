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
import qualified Data.Maybe as Maybe
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
                        , formatExpression False empty expr
                            |> indent 4
                        ]
                    |> margin (if compact then 1 else 2)
                True ->
                    hbox
                        [ formatPattern name
                        , hbox $ List.map (\arg -> hbox [ text " ", formatPattern arg]) args
                        , text " = "
                        , formatExpression False empty expr
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


formatExpression :: Bool -> Box -> AST.Expression.Expr -> Box
formatExpression inList suffix aexpr =
    case RA.drop aexpr of
        AST.Expression.Literal lit ->
            formatCommented formatLiteral lit
        AST.Expression.Var v ->
            hbox2
                (formatCommented formatVar v) -- TODO: comments not tested
                suffix
        AST.Expression.Range _ _ -> text "<range>"
        AST.Expression.ExplicitList exprs False ->
            case exprs of
                [] ->
                    text "[]"
                _ ->
                    hboxlist "[ " ", " " ]" (formatExpression False empty) exprs
        AST.Expression.ExplicitList exprs True ->
            case exprs of
                [] ->
                    text "[]"
                _ ->
                    vboxlist
                        (text "[ ") ", "
                        (text "]")
                        (formatExpression True empty) exprs

        AST.Expression.Binops l ops False ->
            let
                opBoxes (op,e) =
                  [ hspace 1
                  , formatCommented formatInfixVar op
                  , hspace 1
                  , formatExpression False empty e
                  ]
            in
                hbox $
                    formatExpression False empty l
                    : concatMap opBoxes ops
        AST.Expression.Binops l ops True ->
            let
                formatOp (op,e) =
                    hbox
                        [ formatCommented formatInfixVar op -- TODO: comments not tested
                        , hspace 1
                        , formatExpression False empty e
                        ]
            in
                vbox
                    [ formatExpression False empty l
                    , vbox (map formatOp ops)
                        |> indent (if inList then 2 else 4)
                    ]

        AST.Expression.Lambda patterns expr False ->
            hbox
                [ hboxlist "\\" " " " -> " formatPattern patterns
                , formatExpression False empty expr
                ]
        AST.Expression.Lambda patterns expr True ->
            vbox
                [ hboxlist "\\" " " " -> " formatPattern patterns
                , formatExpression False empty expr
                    |> indent (if inList then 2 else 4)
                ]

        AST.Expression.Unary AST.Expression.Negative e ->
            hbox
                [ text "-"
                , formatExpression False empty e
                ]
        AST.Expression.App l rs multiline ->
            case multiline of
                False ->
                    hbox
                        [ formatExpression False empty l
                        , hboxlist " " " " "" (formatExpression False empty) rs
                        ]
                True ->
                    vbox2
                        (formatExpression False empty l)
                        (vboxlist empty "" empty (formatExpression False empty) rs
                            |> indent (if inList then 2 else 4))

        AST.Expression.If [] els ->
            vbox
                [ text "<INVALID IF EXPRESSION>"
                , formatExpression False empty els
                    |> indent (if inList then 2 else 4)
                ]
        AST.Expression.If ((if0,multiline0,body0):elseifs) els ->
            vbox
                [ case multiline0 of
                      False ->
                          hbox
                              [ text "if "
                              , formatExpression False empty if0
                              , text " then"
                              ]
                      True ->
                          vbox
                              [ text "if"
                              , formatExpression False empty if0
                                  |> indent (if inList then 2 else 4)
                              , text "then"
                              ]
                , formatExpression False empty body0
                    |> indent (if inList then 2 else 4)
                , let
                    formatElseIf (if',multiline',body') =
                        vbox
                            [ case multiline' of
                                False ->
                                    hbox
                                        [ text "else if "
                                        , formatExpression False empty if'
                                        , text " then"
                                        ]
                                True ->
                                    vbox
                                        [ text "else if"
                                        , formatExpression False empty if'
                                            |> indent (if inList then 2 else 4)
                                        , text "then"
                                        ]
                            , formatExpression False empty body'
                                |> indent (if inList then 2 else 4)
                            ]
                  in
                      vbox (map formatElseIf elseifs)
                , text "else"
                , formatExpression False empty els
                    |> indent (if inList then 2 else 4)
                ]

        AST.Expression.Let defs expr ->
            vbox
                [ text "let"
                , vboxlist empty "" empty (formatDefinition True) defs
                    |> indent (if inList then 2 else 4)
                    |> margin 0
                , text "in"
                , formatExpression False empty expr
                    |> indent (if inList then 2 else 4)
                ]
        AST.Expression.Case (subject,multilineSubject) clauses ->
            vbox
                [ case multilineSubject of
                    False ->
                        hbox
                            [ text "case "
                            , formatExpression False empty subject
                            , text " of"
                            ]
                    True ->
                        vbox
                            [ text "case"
                            , formatExpression False empty subject
                                |> indent (if inList then 2 else 4)
                            , text "of"
                            ]
                , let
                      formatClause (pat,expr) =
                          vbox
                              [ hbox
                                  [ formatPattern pat
                                  , text " ->"
                                  ]
                              , formatExpression False empty expr
                                  |> indent 4
                              ]
                  in
                      vbox (map formatClause clauses)
                      |> indent (if inList then 2 else 4)
                ]
        AST.Expression.Data _ _ -> text "<expression data>"

        AST.Expression.Tuple exprs False ->
            hboxlist "(" ", " ")" (formatExpression False empty) exprs
        AST.Expression.Tuple exprs True ->
            vboxlist
                (text "( ") ", "
                (text ")")
                (formatExpression True empty) exprs

        AST.Expression.TupleFunction n ->
            hboxlist "(" "" ")" (text . const ",") [0 .. n]

        AST.Expression.Access expr field ->
            formatExpression
                False
                (hbox [ text ".", text field, suffix ])
                expr -- TODO: needs to have parens in some cases

        AST.Expression.AccessFunction field ->
            hbox2 (text ".") (text field)

        AST.Expression.Update base pairs False ->
            let
                pair (k,v,_) = -- multiline' will always be False
                    hbox
                        [ text k
                        , text " = "
                        , formatExpression False empty v
                        ]
            in
                hbox
                    [ text "{ "
                    , formatExpression False empty base
                    , hboxlist " | " ", " "" pair pairs
                    , text " }"
                    , suffix
                    ]
        AST.Expression.Update base pairs True ->
            let
                pair (k,v,multiline') =
                    case multiline' of
                        False ->
                            hbox
                                [ text k
                                , text " = "
                                , formatExpression False empty v
                                ]
                        True ->
                            vbox
                                [ hbox2 (text k) (text " =")
                                , formatExpression False empty v
                                    |> indent 2
                                ]
            in
                vbox
                    [ hbox2 (text "{ ") (formatExpression False empty base)
                    , vboxlist
                        (text "| ") ", "
                        empty
                        pair pairs
                        |> indent 4
                    , (hbox2 (text "}") suffix)
                    ]

        AST.Expression.Record pairs True ->
            let
                pair (k,v,multiline') =
                    case multiline' of
                        False ->
                            hbox
                                [ text k
                                , text " = "
                                , formatExpression False empty v
                                ]
                        True ->
                            vbox
                                [ hbox2 (text k) (text " =")
                                , formatExpression False empty v
                                    |> indent 2
                                ]
            in
                vboxlist
                    (text "{ ") ", "
                    (hbox2 (text "}") suffix)
                    pair pairs
        AST.Expression.Record pairs False ->
            let
                pair (k,v,_) = -- multiline' will always be false
                    hbox
                        [ text k
                        , text " = "
                        , formatExpression False empty v
                        ]
            in
                hbox2
                  (hboxlist
                      "{ " ", "
                      " }"
                      pair pairs)
                  suffix

        AST.Expression.Parens expr False ->
            hbox
                [ text "("
                , formatExpression False empty expr
                , hbox2 (text ")") suffix
                ]
        AST.Expression.Parens expr True->
            vboxlist
                (text "( ") ""
                (hbox2 (text ")") suffix)
                (formatExpression True empty) [expr]

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


formatComment :: Comment -> Box
formatComment comment =
    case comment of
        BlockComment c ->
            text $ "{- " ++ c ++ " -} "


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
                            (empty, text "{ ")
                        Just base ->
                            (hbox2 (text "{ ") (formatType base), text "| ")
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
                        vbox
                            [ vStart
                            , vboxlist vPrefix ", " empty pair fields
                                |> indent (if Maybe.isNothing ext then 0 else 4)
                            , text "}"
                            ]


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
