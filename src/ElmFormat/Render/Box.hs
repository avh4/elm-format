{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Box where

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
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Reporting.Annotation as RA
import Text.Printf (printf)


formatModule :: AST.Module.Module -> Box'
formatModule modu =
    vbox
        [ hbox
            [ text "module "
            , formatName $ AST.Module.name modu
            , case formatListing $ AST.Module.exports modu of
                Just listing ->
                    case isLine listing of
                        Just listing' ->
                            depr $ line $ row [ space, listing' ]
                        _ ->
                            text "<TODO: multiline module listing>"
                _ ->
                    empty
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


formatModuleDocs :: RA.Located (Maybe String) -> Box'
formatModuleDocs adocs =
    case RA.drop adocs of
        Nothing ->
            empty
        Just docs ->
            formatDocComment docs
            |> margin 1


formatDocComment :: String -> Box'
formatDocComment docs =
    hbox
        [ text "{-| "
        , text $ docs
        , text "-}"
        ]


formatName :: MN.Raw -> Box'
formatName name =
    text (List.intercalate "." name)


formatImport :: AST.Module.UserImport -> Box'
formatImport aimport =
    case RA.drop aimport of
        (name,method) ->
            hbox
                [ text "import "
                , formatName name
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
                                , hjoin (text ", ") (map (depr . formatVarValue) vars)
                                , text ")"
                                ]
                        AST.Variable.Listing _ True ->
                            text "<NOT POSSIBLE?>"


formatListing :: AST.Variable.Listing (RA.Located AST.Variable.Value)-> Maybe Box
formatListing listing =
    case listing of
        AST.Variable.Listing [] False ->
            Nothing
        AST.Variable.Listing _ True -> -- Not possible for first arg to be non-empty
            Just $ line $ keyword "(..)"
        AST.Variable.Listing vars False ->
            Just $ elmGroup False "(" "," ")" False $ map (formatVarValue . RA.drop) vars


formatStringListing :: AST.Variable.Listing String -> Maybe Line
formatStringListing listing =
    case listing of
        AST.Variable.Listing [] False ->
            Nothing
        AST.Variable.Listing _ True -> -- Not possible for first arg to be non-empty
            Just $ keyword "(..)"
        AST.Variable.Listing vars False ->
            Just $ row
                [ punc "("
                , row $ List.intersperse (punc ",") $ map identifier vars
                , punc ")"
                ]


formatVarValue :: AST.Variable.Value -> Box
formatVarValue aval =
    case aval of
        AST.Variable.Value val ->
            formatCommented (line . formatVar) val -- TODO: comments not tested
        AST.Variable.Alias name ->
            line $ identifier name
        AST.Variable.Union name listing ->
            case formatStringListing listing of
                Just listing' ->
                    line $ row
                        [ identifier name
                        , listing'
                        ]
                Nothing ->
                    line $ identifier name


formatDeclaration :: AST.Declaration.Decl -> Box'
formatDeclaration decl =
    case decl of
        AST.Declaration.Comment docs ->
            formatDocComment docs
        AST.Declaration.Decl adecl ->
            case RA.drop adecl of
                AST.Declaration.Definition def ->
                    formatDefinition False def
                AST.Declaration.Datatype name args ctors ->
                    vbox
                        [ hbox
                            [ text "type "
                            , text name
                            , hboxlist " " " " "" text args
                            ]
                        , vboxlist
                            (text "= ") "| "
                            empty
                            (\(c, args') -> hbox2 (text c) (hboxlist " " " " "" (depr . formatType' ForCtor) args')) ctors
                            |> indent' 4
                        ]
                        |> margin 2
                AST.Declaration.TypeAlias name args typ ->
                    vbox
                        [ hboxlist "type alias " " " " =" text (name:args)
                        , (depr . formatType) typ
                            |> indent' 4
                        ]
                        |> margin 2
                AST.Declaration.PortAnnotation name typ ->
                    hbox
                        [ text "port "
                        , text name
                        , text " : "
                        , (depr . formatType) typ
                        ]
                AST.Declaration.PortDefinition name expr ->
                    vbox
                        [ hbox
                            [ text "port "
                            , text name
                            , text " ="
                            ]
                        , formatExpression False empty expr
                            |> indent' 4
                        ]
                        |> margin 2
                AST.Declaration.Fixity assoc precedence name ->
                    hbox
                        [ case assoc of
                              AST.Declaration.L -> text "infixl"
                              AST.Declaration.R -> text "infixr"
                              AST.Declaration.N -> text "infix"
                        , text " "
                        , text $ show precedence
                        , text " "
                        , depr $ formatCommented (line . formatInfixVar) name
                        ]


formatDefinition :: Bool -> AST.Expression.Def -> Box'
formatDefinition compact adef =
    case RA.drop adef of
        AST.Expression.Definition name args expr multiline ->
            case compact && (not multiline) of
                False ->
                    vbox
                        [ hbox
                            [ depr $ formatPattern True name
                            , hbox $ List.map (\arg -> hbox [ text " ", depr $ formatPattern True arg]) args
                            , text " ="
                            ]
                        , formatExpression False empty expr
                            |> indent' 4
                        ]
                    |> margin (if compact then 1 else 2)
                True ->
                    hbox
                        [ depr $ formatPattern True name
                        , hbox $ List.map (\arg -> hbox [ text " ", depr $ formatPattern True arg]) args
                        , text " = "
                        , formatExpression False empty expr
                        ]
                    |> margin 1
        AST.Expression.TypeAnnotation name typ ->
            hbox
                [ depr $ formatCommented (line . formatVar) name -- TODO: comments not tested
                , text " : "
                , (depr . formatType) typ
                ]


formatPattern :: Bool -> AST.Pattern.Pattern -> Box
formatPattern parensRequired apattern =
    case RA.drop apattern of
        AST.Pattern.Data ctor patterns ->
            elmApplication
                (line $ formatVar ctor)
                (map (formatPattern True) patterns)
        AST.Pattern.Tuple patterns ->
            elmGroup True "(" "," ")" False $ map (formatPattern False) patterns
        AST.Pattern.Record fields ->
            elmGroup True "{" "," "}" False $ map (line . identifier) fields
        AST.Pattern.Alias name pattern ->
            case isLine $ formatPattern True pattern of
                Just pattern' ->
                    line $ row
                        [ pattern'
                        , space
                        , keyword "as"
                        , space
                        , identifier name
                        ]
                _ -> -- TODO
                    line $ keyword "<TODO-multiline PAttern alias>"
            |> (if parensRequired then addParens else id)
        AST.Pattern.Var var ->
            formatCommented (line . formatVar) var -- TODO: comments not tested
        AST.Pattern.Anything ->
            line $ keyword "_"
        AST.Pattern.Literal lit ->
            formatLiteral lit


formatExpression :: Bool -> Box' -> AST.Expression.Expr -> Box'
formatExpression inList suffix aexpr =
    case RA.drop aexpr of
        AST.Expression.Literal lit ->
            depr $ formatCommented (formatLiteral) lit
        AST.Expression.Var v ->
            hbox2
                (depr $ formatCommented (line . formatVar) v) -- TODO: comments not tested
                suffix

        AST.Expression.Range left right False ->
            hbox
                [ text "["
                , formatExpression False empty left
                , text ".."
                , formatExpression False empty right
                , text "]"
                ]
        AST.Expression.Range left right True ->
            vbox
                [ text "["
                , formatExpression False empty left
                    |> indent' (if inList then 2 else 4)
                , text ".."
                , formatExpression False empty right
                    |> indent' (if inList then 2 else 4)
                , text "]"
                ]


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
                  , depr $ formatCommented (line. formatInfixVar) op
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
                    let
                        fop = depr $ formatCommented (line . formatInfixVar) op -- TODO: comments not tested
                        fexp = formatExpression True empty e
                        space =
                            if height fexp <= 1 then
                                1
                            else if width fop `mod` 2 == 1 then
                                1
                            else
                                2
                    in
                        hbox
                            [ fop
                            , hspace space
                            , fexp
                            ]
            in
                vbox
                    [ formatExpression False empty l
                    , vbox (map formatOp ops)
                        |> indent' (if inList then 2 else 4)
                    ]

        AST.Expression.Lambda patterns expr False ->
            hbox
                [ hboxlist "\\" " " " -> " (depr . formatPattern True) patterns
                , formatExpression False empty expr
                ]
        AST.Expression.Lambda patterns expr True ->
            vbox
                [ hboxlist "\\" " " " -> " (depr . formatPattern True) patterns
                , formatExpression False empty expr
                    |> indent' (if inList then 2 else 4)
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
                            |> indent' (if inList then 2 else 4))

        AST.Expression.If [] els ->
            vbox
                [ text "<INVALID IF EXPRESSION>"
                , formatExpression False empty els
                    |> indent' (if inList then 2 else 4)
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
                                  |> indent' (if inList then 2 else 4)
                              , text "then"
                              ]
                , formatExpression False empty body0
                    |> indent' (if inList then 2 else 4)
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
                                            |> indent' (if inList then 2 else 4)
                                        , text "then"
                                        ]
                            , formatExpression False empty body'
                                |> indent' (if inList then 2 else 4)
                            ]
                  in
                      vbox (map formatElseIf elseifs)
                , text "else"
                , formatExpression False empty els
                    |> indent' (if inList then 2 else 4)
                ]

        AST.Expression.Let defs expr ->
            vbox
                [ text "let"
                , vboxlist empty "" empty (formatDefinition True) defs
                    |> indent' (if inList then 2 else 4)
                    |> margin 0
                , text "in"
                , formatExpression False empty expr
                    |> indent' (if inList then 2 else 4)
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
                                |> indent' (if inList then 2 else 4)
                            , text "of"
                            ]
                , let
                      formatClause (pat,expr) =
                          vbox
                              [ hbox
                                  [ depr $ formatPattern True pat
                                  , text " ->"
                                  ]
                              , formatExpression False empty expr
                                  |> indent' 4
                              ]
                              |> margin 1
                  in
                      vbox (map formatClause clauses)
                      |> margin 0
                      |> indent' (if inList then 2 else 4)
                ]

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
                                    |> indent' 2
                                ]
            in
                vbox
                    [ hbox2 (text "{ ") (formatExpression False empty base)
                    , vboxlist
                        (text "| ") ", "
                        empty
                        pair pairs
                        |> indent' 4
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
                                    |> indent' 2
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

        AST.Expression.GLShader _ _ _ -> text "<glshader>"


formatCommented :: (a -> Box) -> Commented a -> Box
formatCommented format commented =
    case commented of
        Commented comments inner ->
            case
                ( allSingles $ map formatComment comments
                , isLine $ format inner
                )
            of
                (Just [], Just inner') ->
                    line inner'
                (Just cs, Just inner') ->
                    line $ row
                        [ row cs
                        , space
                        , inner'
                        ]
                _ ->
                    stack
                        [ stack $ map formatComment comments
                        , format inner
                        ]


formatComment :: Comment -> Box
formatComment comment =
    case comment of
        BlockComment c ->
            case lines c of
                (l:[]) ->
                    line $ row
                        [ punc "{-"
                        , space
                        , literal l
                        , space
                        , punc "-}"
                        ]
                (l1:ls) -> -- TODO: not tested
                    stack
                        [ line $ row
                            [ punc "{-"
                            , space
                            , literal l1
                            ]
                        , stack $ map (line . literal) ls
                        , line $ punc "-}"
                        ]


formatLiteral :: L.Literal -> Box
formatLiteral lit =
    case lit of
        L.IntNum i ->
            line $ literal $ show i
        L.FloatNum f ->
            line $ literal $ show f
        L.Chr c ->
            formatString SChar [c]
        L.Str s multi ->
            formatString (if multi then SMulti else SString) s
        L.Boolean True ->
            line $ literal "True"
        L.Boolean False ->
            line $ literal "False" -- TODO: not tested


data StringStyle
    = SChar
    | SString
    | SMulti
    deriving (Eq)


formatString :: StringStyle -> String -> Box
formatString style s =
    let
        hex c =
            if Char.ord c <= 0xFF then
                "\\x" ++ (printf "%02X" $ Char.ord c)
            else
                "\\x" ++ (printf "%04X" $ Char.ord c)

        fix c =
            if (style == SMulti) && c == '\n' then
                [c]
            else if c == '\n' then
                "\\n"
            else if c == '\t' then
                "\\t"
            else if c == '\\' then
                "\\\\"
            else if (style == SString) && c == '\"' then
                "\\\""
            else if (style == SChar) && c == '\'' then
                "\\\'"
            else if not $ Char.isPrint c then
                hex c
            else if c == ' ' then
                [c]
            else if Char.isSpace c then
                hex c
            else
                [c]
    in
        case style of
            SChar ->
                line $ row
                    [ punc "\'"
                    , literal $ concatMap fix s
                    , punc "\'"
                    ]
            SString ->
                line $ row
                    [ punc "\""
                    , literal $ concatMap fix s
                    , punc "\""
                    ]
            SMulti ->
                line $ literal $ "\"\"\"" ++ concatMap fix s ++ "\"\"\""


data TypeParensRequired
    = ForLambda
    | ForCtor
    | NotRequired
    deriving (Eq)


formatType :: AST.Type.Type -> Box
formatType =
    formatType' NotRequired


wrap :: Line -> Line -> [Line] -> Line -> Box
wrap left first rest right =
    stack
        [ line $ row [left, first]
        , stack (map (\l -> line $ row [space, space, l]) rest)
        , line $ right
        ]


addParens :: Box -> Box
addParens b =
    case destructure b of
        (l, []) ->
            line $ row
                [ punc "("
                , l
                , punc ")"
                ]
        (first, rest) ->
            wrap (punc "(") first rest (punc ")")


commaSpace :: Line
commaSpace =
    row
        [ punc ","
        , space
        ]


formatType' :: TypeParensRequired -> AST.Type.Type -> Box
formatType' requireParens atype =
    case RA.drop atype of
        AST.Type.RLambda first rest ->
            case
                allSingles $ map (formatType' ForLambda) (first:rest)
            of
                Just typs ->
                    line $ row $ List.intersperse (row [ space, keyword "->", space]) typs
                _ ->
                    stack
                        [ formatType' ForLambda first
                        , rest
                            |> map (formatType' ForLambda)
                            |> map (prefix (keyword "->"))
                            |> stack
                        ]
            |> (if requireParens /= NotRequired then addParens else id)
        AST.Type.RVar var ->
            line $ identifier var
        AST.Type.RType var ->
            line $ formatVar var
        AST.Type.RApp ctor args ->
            elmApplication
                (formatType' ForCtor ctor)
                (map (formatType' ForCtor) args)
                |> (if requireParens == ForCtor then addParens else id)
        AST.Type.RTuple (first:rest) ->
            elmGroup True "(" "," ")" False (map formatType (first:rest))
        AST.Type.RTuple [] ->
            line $ keyword "()"
        AST.Type.RRecord ext fields multiline ->
            let
                formatField (name, typ, multiline') =
                    case (multiline', destructure (formatType typ)) of
                        (False, (first, [])) ->
                            line $ row
                                [ identifier name
                                , space
                                , punc ":"
                                , space
                                , first
                                ]
                        _ ->
                            stack
                                [ line $ row
                                    [ identifier name
                                    , space
                                    , punc ":"
                                    ]
                                , formatType typ
                                    |> indent
                                ]
            in
                case (ext, fields) of
                    (Just _, []) ->
                        line $ keyword "<INVALID RECORD EXTENSION>"
                    (Just typ, first:rest) ->
                        case
                            ( multiline
                            , isLine $ formatType typ
                            , allSingles $ map formatField fields
                            )
                        of
                            (False, Just typ', Just fields') ->
                                line $ row
                                    [ punc "{"
                                    , space
                                    , typ'
                                    , space
                                    , punc "|"
                                    , space
                                    , row (List.intersperse commaSpace fields')
                                    , space
                                    , punc "}"
                                    ]
                            _ ->
                                stack
                                    [ prefix (punc "{") (formatType typ)
                                    , stack
                                        ([ prefix (punc "|") $ formatField first ]
                                        ++ (map (prefix (punc ",") . formatField) rest))
                                        |> indent
                                    , line $ punc "}"
                                    ]
                    (Nothing, _) ->
                        elmGroup True "{" "," "}" multiline (map formatField fields)


formatVar :: AST.Variable.Ref -> Line
formatVar var =
    case var of
        AST.Variable.VarRef name ->
            identifier name
        AST.Variable.OpRef name ->
            identifier $ "(" ++ name ++ ")"
        AST.Variable.WildcardRef ->
            keyword "_" -- TODO: not tested


formatInfixVar :: AST.Variable.Ref -> Line
formatInfixVar var =
    case var of
        AST.Variable.VarRef name ->
            identifier $ "`" ++ name ++ "`" -- TODO: not tested
        AST.Variable.OpRef name ->
            identifier name
        AST.Variable.WildcardRef ->
            keyword "_" -- TODO: should never happen
