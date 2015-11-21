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
import Util.List


splitWhere :: (a -> Bool) -> [a] -> [[a]]
splitWhere predicate list =
    let
        merge acc result =
            (reverse acc):result

        step (acc,result) next =
            if predicate next then
                ([], merge (next:acc) result)
            else
                (next:acc, result)
    in
      list
          |> foldl step ([],[])
          |> uncurry merge
          |> dropWhile null
          |> reverse


isDeclaration :: AST.Declaration.Decl -> Bool
isDeclaration decl =
    case decl of
        AST.Declaration.Decl adecl ->
            case RA.drop adecl of
                AST.Declaration.Definition def ->
                    case RA.drop def of
                        AST.Expression.Definition _ _ _ _ _ ->
                            True

                        _ ->
                            False

                AST.Declaration.Datatype _ _ _ ->
                    True

                AST.Declaration.TypeAlias _ _ _ ->
                    True

                AST.Declaration.PortDefinition _ _ ->
                    True

                _ ->
                    False

        _ ->
            False

formatModule :: AST.Module.Module -> Box
formatModule modu =
    let
        moduleLine =
            line $ row
                [ keyword "module"
                , space
                , formatName $ AST.Module.name modu
                , case formatListing $ fmap RA.drop $ AST.Module.exports modu of
                    Just listing ->
                        case isLine listing of
                            Right listing' ->
                                row [ space, listing' ]
                            _ ->
                                keyword "<TODO: multiline module listing>"
                    _ ->
                        keyword "<TODO: no listing>"
                , space
                , keyword "where"
                ]

        docs =
            formatModuleDocs (AST.Module.docs modu)

        imports =
              AST.Module.imports modu
                  |> List.sortOn (fst . RA.drop)
                  |> map formatImport

        isComment d =
            case d of
                AST.Declaration.BodyComment _ ->
                    True
                _ ->
                    False

        spacer first second =
            case (isDeclaration first, isComment first, isComment second) of
                (_, False, True) ->
                    List.replicate 3 blankLine
                (True, _, _) ->
                    List.replicate 2 blankLine
                (False, True, False) ->
                    List.replicate 2 blankLine
                _ ->
                    []

        body =
            stack $
                intersperseMap spacer formatDeclaration $
                    AST.Module.body modu
    in
        case (docs, imports) of -- TODO: not all cases are tested
            (Just docs', []) ->
                stack
                    [ moduleLine
                    , blankLine
                    , docs'
                    , blankLine
                    , blankLine
                    , body
                    ]
            (Just docs', _) ->
                stack
                    [ moduleLine
                    , blankLine
                    , docs'
                    , blankLine
                    , stack imports
                    , blankLine
                    , blankLine
                    , body
                    ]
            (Nothing, []) ->
                stack
                    [ moduleLine
                    , blankLine
                    , blankLine
                    , body
                    ]
            (Nothing, _) ->
                stack
                    [ moduleLine
                    , blankLine
                    , stack imports
                    , blankLine
                    , blankLine
                    , body
                    ]


formatModuleDocs :: RA.Located (Maybe String) -> Maybe Box
formatModuleDocs adocs =
    case RA.drop adocs of
        Nothing ->
            Nothing
        Just docs ->
            Just $ formatDocComment docs


formatDocComment :: String -> Box
formatDocComment docs =
    case lines docs of
        [] ->
            line $ row [ punc "{-|", space, punc "-}" ]
        (first:[]) ->
            stack
                [ line $ row [ punc "{-|", space, literal first ]
                , line $ punc "-}"
                ]
        (first:rest) ->
            stack
                [ line $ row [ punc "{-|", space, literal first ]
                , stack $ map (line . literal) rest
                , line $ punc "-}"
                ]


formatName :: MN.Raw -> Line
formatName name =
    identifier (List.intercalate "." name)


formatImport :: AST.Module.UserImport -> Box
formatImport aimport =
    case RA.drop aimport of
        (name,method) ->
            let
                as =
                    case AST.Module.alias method of
                        Nothing ->
                            formatName name
                        Just alias ->
                            row
                                [ formatName name
                                , space
                                , keyword "as"
                                , space
                                , identifier alias
                                ]
            in
                case formatListing $ AST.Module.exposedVars method of
                    Just listing ->
                        case isLine listing of
                            Right listing' ->
                                line $ row
                                    [ keyword "import"
                                    , space
                                    , as
                                    , space
                                    , keyword "exposing"
                                    , space
                                    , listing'
                                    ]
                            _ ->
                                line $ keyword "<TODO: multiline exposing>"
                    Nothing ->
                        line $ row
                            [ keyword "import"
                            , space
                            , as
                            ]


formatListing :: AST.Variable.Listing AST.Variable.Value-> Maybe Box
formatListing listing =
    case listing of
        AST.Variable.Listing [] False ->
            Nothing
        AST.Variable.Listing _ True -> -- Not possible for first arg to be non-empty
            Just $ line $ keyword "(..)"
        AST.Variable.Listing vars False ->
            Just $ elmGroup False "(" "," ")" False $ map formatVarValue vars


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
                , row $ List.intersperse (row [punc ",", space]) $ map identifier vars
                , punc ")"
                ]


formatVarValue :: AST.Variable.Value -> Box
formatVarValue aval =
    case aval of
        AST.Variable.Value val ->
            line $ formatVar val

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


isDefinition :: AST.Expression.Def' -> Bool
isDefinition def =
    case def of
        AST.Expression.Definition _ _ _ _ _ ->
            True
        _ ->
            False


formatDeclaration :: AST.Declaration.Decl -> Box
formatDeclaration decl =
    case decl of
        AST.Declaration.DocComment docs ->
            formatDocComment docs

        AST.Declaration.BodyComment c ->
            formatComment c

        AST.Declaration.Decl adecl ->
            case RA.drop adecl of
                AST.Declaration.Definition def ->
                    formatDefinition False def

                AST.Declaration.Datatype name args ctors ->
                    let
                        ctor (tag,args') =
                            case allSingles $ map (formatType' ForCtor) args' of
                                Right args'' ->
                                    line $ row $ List.intersperse space $ (identifier tag):args''
                                _ ->
                                    line $ keyword "<TODO: multiline type in constructor argument>"
                    in
                        case ctors of
                            [] ->
                                line $ keyword "<INVALID DATA TYPE: no constructors>"
                            (first:rest) ->
                                stack
                                    [ line $ row
                                        [ keyword "type"
                                        , space
                                        , row $ List.intersperse space $ map identifier (name:args)
                                        ]
                                    , stack
                                        [ ctor first
                                            |> prefix (row [punc "=", space])
                                        , stack $ map (prefix (row [punc "|", space]) . ctor) rest
                                        ]
                                        |> indent
                                    ]

                AST.Declaration.TypeAlias name args typ ->
                    stack
                        [ line $ row
                            [ keyword "type"
                            , space
                            , keyword "alias"
                            , space
                            , row $ List.intersperse space $ map identifier $ name:args
                            , space
                            , punc "="
                            ]
                        , formatType typ
                            |> indent
                        ]

                AST.Declaration.PortAnnotation name typ ->
                    case isLine $ formatType typ of
                        Right typ' ->
                            line $ row
                                [ keyword "port"
                                , space
                                , identifier name
                                , space
                                , punc ":"
                                , space
                                , typ'
                                ]
                        _ ->
                            line $ keyword "<TODO: multiline type in port annotation>"

                AST.Declaration.PortDefinition name expr ->
                    stack
                        [ line $ row
                            [ keyword "port"
                            , space
                            , identifier name
                            , space
                            , punc "="
                            ]
                        , formatExpression expr
                            |> indent
                        ]

                AST.Declaration.Fixity assoc precedence name ->
                    case isLine $ (line $ formatInfixVar name) of
                        Right name' ->
                            line $ row
                                [ case assoc of
                                      AST.Declaration.L -> keyword "infixl"
                                      AST.Declaration.R -> keyword "infixr"
                                      AST.Declaration.N -> keyword "infix"
                                , space
                                , literal $ show precedence
                                , space
                                , name'
                                ]
                        _ ->
                            line $ keyword "<TODO: multiline name in fixity declaration>"


formatDefinition :: Bool -> AST.Expression.Def -> Box
formatDefinition compact adef =
    case RA.drop adef of
        AST.Expression.Definition name args comments expr multiline ->
            case
                ( compact
                , multiline
                , isLine $ formatPattern True name
                , allSingles $ map (formatPattern True) args
                , comments
                , isLine $ formatExpression expr
                )
            of
                (True, False, Right name', Right args', [], Right expr') ->
                    line $ row
                        [ row $ List.intersperse space $ (name':args')
                        , space
                        , punc "="
                        , space
                        , expr'
                        ]
                (_, _, Right name', Right args', _, _) ->
                    stack $
                        [ line $ row
                            [ row $ List.intersperse space $ (name':args')
                            , space
                            , punc "="
                            ]
                        , indent $ stack $
                            (map formatComment comments)
                            ++ [ formatExpression expr ]
                        ]
                _ ->
                    line $ keyword "<TODO: let binding with multiline name/pattern>"

        AST.Expression.TypeAnnotation name typ ->
            case
                ( isLine $ (line $ formatVar name)
                , isLine $ formatType typ
                )
            of
                (Right name', Right typ') ->
                    line $ row
                        [ name'
                        , space
                        , punc ":"
                        , space
                        , typ'
                        ]

                (Right name', Left typ') ->
                    stack
                        [ line $ row [ name', space, punc ":" ]
                        , typ'
                            |> indent
                        ]

                _ ->
                    line $ keyword "<TODO: multiline name in type annotation>"

        AST.Expression.LetComment comment ->
            formatComment comment


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
                Right pattern' ->
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
            line $ formatVar var
        AST.Pattern.Anything ->
            line $ keyword "_"
        AST.Pattern.Literal lit ->
            formatLiteral lit


addSuffix :: Line -> Box -> Box
addSuffix suffix b =
    case destructure b of
        (l,[]) ->
            line $ row [ l, suffix ]
        (l1,ls) ->
            stack $
                [ line l1 ]
                ++ (map line $ init ls)
                ++ [ line $ row [ last ls, suffix ] ]


formatRecordPair :: (String, AST.Expression.Expr, Bool) -> Box
formatRecordPair (k,v,multiline') =
    case
        ( multiline'
        , isLine $ formatExpression v
        )
    of
        (False, Right v') ->
            line $ row
                [ identifier k
                , space
                , punc "="
                , space
                , v'
                ]
        _ ->
            stack
                [ line $ row [ identifier k, space, punc "=" ]
                , formatExpression v
                    |> indent
                ]


formatExpression :: AST.Expression.Expr -> Box
formatExpression aexpr =
    case RA.drop aexpr of
        AST.Expression.Literal lit ->
            formatLiteral lit

        AST.Expression.Var v ->
            line $ formatVar v

        AST.Expression.Range left right multiline ->
            case
                ( multiline
                , isLine $ formatCommented formatExpression left
                , isLine $ formatCommented formatExpression right
                )
            of
                (False, Right left', Right right') ->
                    line $ row
                        [ punc "["
                        , left'
                        , punc ".."
                        , right'
                        , punc "]"
                        ]
                _ ->
                    stack
                        [ line $ punc "["
                        , formatCommented formatExpression left
                            |> indent
                        , line $ punc ".."
                        , formatCommented formatExpression right
                            |> indent
                        , line $ punc "]"
                        ]

        AST.Expression.ExplicitList exprs multiline ->
            elmGroup True "[" "," "]" multiline $ map formatExpression exprs

        AST.Expression.Binops left ops multiline ->
            let
                left' =
                    formatExpression left

                ops' =
                    ops |> map fst |> map (formatCommented (line. formatInfixVar)) -- TODO: comments not

                exprs =
                    ops |> map snd |> map (formatCommented formatExpression)
            in
                case
                    ( multiline
                    , isLine $ left'
                    , allSingles $ ops'
                    , allSingles $ exprs
                    )
                of
                    (False, Right left'', Right ops'', Right exprs') ->
                        zip ops'' exprs'
                            |> map (\(op,e) -> row [op, space, e])
                            |> List.intersperse space
                            |> (:) space
                            |> (:) left''
                            |> row
                            |> line
                    (_, _, Right ops'', _) ->
                        zip ops'' exprs
                            |> map (\(op,e) -> prefix (row [op, space]) e)
                            |> stack
                            |> (\body -> stack [left', indent body])
                    _ ->
                        zip ops' exprs
                            |> map (\(op,e) -> stack [ op, indent e ])
                            |> stack
                            |> (\body -> stack [left', indent body])

        AST.Expression.Lambda patterns expr multiline ->
            case
                ( multiline
                , allSingles $ map (formatPattern True) patterns
                , isLine $ formatExpression expr
                )
            of
                (False, Right patterns', Right expr') ->
                    line $ row
                        [ punc "\\"
                        , row $ List.intersperse space $ patterns'
                        , space
                        , punc "->"
                        , space
                        , expr'
                        ]
                (_, Right patterns', _) ->
                    stack
                        [ line $ row
                            [ punc "\\"
                            , row $ List.intersperse space $ patterns'
                            , space
                            , punc "->"
                            ]
                        , formatExpression expr
                            |> indent
                        ]
                _ ->
                    line $ keyword "<TODO: multiline pattern in lambda>"

        AST.Expression.Unary AST.Expression.Negative e ->
            prefix (punc "-") $ formatExpression e

        AST.Expression.App left args multiline ->
            case
                ( multiline
                , isLine $ formatExpression left
                , allSingles $ map (formatCommented formatExpression) args
                )
            of
                (False, Right left', Right args') ->
                  line $ row
                      $ List.intersperse space $ (left':args')
                _ ->
                    stack
                        [ formatExpression left
                        , args
                            |> map (formatCommented formatExpression)
                            |> stack
                            |> indent
                        ]

        AST.Expression.If [] els ->
            stack
                [ line $ keyword "<INVALID IF EXPRESSION>"
                , formatExpression els
                    |> indent
                ]
        AST.Expression.If (if':elseifs) els ->
            let
                opening key multiline cond =
                    case (multiline, isLine cond) of
                        (False, Right cond') ->
                            line $ row
                                [ keyword key
                                , space
                                , cond'
                                , space
                                , keyword "then"
                                ]
                        _ ->
                            stack
                                [ line $ keyword key
                                , cond |> indent
                                , line $ keyword "then"
                                ]

                clause key (cond,multiline,body) =
                    stack
                        [ opening key multiline $ formatExpression cond
                        , formatExpression body
                            |> indent
                        ]
            in
                stack $
                    [ clause "if" if']
                    ++ ( elseifs |> map (clause "else if") )
                    ++
                    [ line $ keyword "else"
                    , formatExpression els
                        |> indent
                    ]

        AST.Expression.Let defs bodyComments expr ->
            let
                spacer first second =
                    case isDefinition $ RA.drop first of
                        True ->
                            [ blankLine ]
                        False ->
                            []
            in
                stack
                    [ line $ keyword "let"
                    , defs
                        |> intersperseMap spacer (formatDefinition True)
                        |> stack
                        |> indent
                    , line $ keyword "in"
                    , indent $ stack $
                        (map formatComment bodyComments)
                        ++ [formatExpression expr]
                    ]

        AST.Expression.Case (subject,multiline) clauses ->
            let
                opening =
                  case
                      ( multiline
                      , isLine $ formatExpression subject
                      )
                  of
                      (False, Right subject') ->
                          line $ row
                              [ keyword "case"
                              , space
                              , subject'
                              , space
                              , keyword "of"
                              ]
                      _ ->
                          stack
                              [ line $ keyword "case"
                              , formatExpression subject
                                  |> indent
                              , line $ keyword "of"
                              ]

                clause (patternComments, pat, bodyComments, expr) =
                    case isLine $ formatPattern True pat of
                        Right pat' ->
                            stack $
                                (map formatComment patternComments)
                                ++
                                [ line $ row [ pat', space, keyword "->"]
                                , indent $ stack $
                                    (map formatComment bodyComments)
                                    ++ [ formatExpression expr ]
                                ]
                        _ ->
                            line $ keyword "<TODO: multiline case pattern>"
            in
                stack
                    [ opening
                    , clauses
                        |> map clause
                        |> List.intersperse blankLine
                        |> stack
                        |> indent
                    ]

        AST.Expression.Tuple exprs multiline ->
            elmGroup True "(" "," ")" multiline $ map (formatCommented formatExpression) exprs

        AST.Expression.TupleFunction n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"

        AST.Expression.Access expr field ->
            formatExpression expr -- TODO: needs to have parens in some cases
                |> addSuffix (row $ [punc ".", identifier field])

        AST.Expression.AccessFunction field ->
            line $ identifier $ "." ++ field

        AST.Expression.RecordUpdate base pairs multiline ->
            case
                ( multiline
                , isLine $ formatExpression base
                , allSingles $ map formatRecordPair pairs
                )
            of
                (False, Right base', Right pairs') ->
                    line $ row
                        [ punc "{"
                        , space
                        , base'
                        , space
                        , punc "|"
                        , space
                        , row $ List.intersperse commaSpace pairs'
                        , space
                        , punc "}"
                        ]
                _ ->
                    case map formatRecordPair pairs of
                        [] ->
                            line $ keyword "<INVALID RECORD EXTENSION>"
                        (first:rest) ->
                            stack
                                [ formatExpression base
                                    |> prefix (row [punc "{", space])
                                , stack
                                    [ prefix (row [punc "|", space]) first
                                    , stack $ map (prefix (row [punc ",", space])) rest
                                    ]
                                    |> indent
                                , line $ punc "}"
                                ]

        AST.Expression.Record pairs multiline ->
            case
                ( multiline
                , allSingles $ map formatRecordPair pairs
                )
            of
                (False, Right pairs') ->
                    line $ row
                        [ punc "{"
                        , space
                        , row $ List.intersperse commaSpace pairs'
                        , space
                        , punc "}"
                        ]
                _ ->
                    elmGroup True "{" "," "}" multiline $ map formatRecordPair pairs

        AST.Expression.Parens expr multiline ->
            case (multiline, isLine $ formatExpression expr) of
                (False, Right expr') ->
                    line $ row [ punc "(", expr', punc ")" ]
                _ ->
                    stack
                        [ formatExpression expr
                            |> prefix (punc "(")
                        , line $ punc ")"
                        ]

        AST.Expression.Unit ->
            line $ punc "()"

        AST.Expression.GLShader _ _ _ ->
            line $ keyword "<TODO: glshader>"


formatCommented :: (a -> Box) -> Commented a -> Box
formatCommented format (Commented pre post inner) =
    case
        ( allSingles $ fmap formatComment pre
        , allSingles $ fmap formatComment post
        , isLine $ format inner
        )
    of
        ( Right pre', Right post', Right inner' ) ->
            line $ row $ List.intersperse space $ concat [pre', [inner'], post']
        _ -> -- TODO: not tested
            stack $
                (map formatComment pre)
                ++ [ format inner ]
                ++ ( map formatComment post)


formatComment :: Comment -> Box
formatComment comment =
    case comment of
        BlockComment c ->
            case lines c of -- TODO: can [] happen with empty string?
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
        LineComment c ->
            mustBreak $ row [ punc "--", literal c ]


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
                Right typs ->
                    line $ row $ List.intersperse (row [ space, keyword "->", space]) typs
                _ -> -- TODO: not tested
                    stack
                        [ formatType' ForLambda first
                        , rest
                            |> map (formatType' ForLambda)
                            |> map (prefix (row [keyword "->", space]))
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
                            (False, Right typ', Right fields') ->
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
                                    [ prefix (row [punc "{", space]) (formatType typ)
                                    , stack
                                        ([ prefix (row [punc "|", space]) $ formatField first ]
                                        ++ (map (prefix (row [punc ",", space]) . formatField) rest))
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
