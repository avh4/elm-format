{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Box where

import Elm.Utils ((|>))
import Box
import Data.Version (showVersion)

import AST.V0_16
import qualified AST.Declaration
import qualified AST.Expression
import qualified AST.Module
import qualified AST.Module.Name as MN
import qualified AST.Pattern
import qualified AST.Variable
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Paths_elm_format as This
import qualified Reporting.Annotation as RA
import Text.Printf (printf)
import Util.List


pleaseReport' :: String -> String -> Line
pleaseReport' what details =
    keyword $ "<elm-format-" ++ (showVersion This.version) ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"


pleaseReport :: String -> String -> Box
pleaseReport what details =
    line $ pleaseReport' what details


surround :: Char -> Char -> Box -> Box
surround left right b =
  let
    left' = punc (left : [])
    right' = punc (right : [])
  in
    case isLine b of
      Right b' ->
          line $ row [ left', b', right' ]
      _ ->
          stack1
              [ b
                  |> prefix left'
              , line $ right'
              ]


parens :: Box -> Box
parens = surround '(' ')'


formatBinary :: Bool -> Box -> [ ( Box, Box ) ] -> Box
formatBinary multiline left ops =
    case
        ( ops
        , multiline
        , isLine $ left
        , allSingles $ map fst ops
        , allSingles $ map snd ops
        )
    of
        ([], _, _, _, _) ->
            pleaseReport "INVALID BINARY EXPRESSION" "no operators"

        (_, False, Right left'', Right ops'', Right exprs') ->
            zip ops'' exprs'
                |> map (\(op,e) -> row [op, space, e])
                |> List.intersperse space
                |> (:) space
                |> (:) left''
                |> row
                |> line

        (_, _, _, Right ops'', _) ->
            zip ops'' (map snd ops)
                |> map (\(op,e) -> prefix (row [op, space]) e)
                |> stack1
                |> (\body -> stack1 [left, indent body])

        _ ->
            ops
                |> map (\(op,e) -> stack1 [ op, indent e ])
                |> stack1
                |> (\body -> stack1 [left, indent body])


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

                AST.Declaration.PortDefinition _ _ _ ->
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
                                pleaseReport' "TODO" "multiline module listing"
                    _ ->
                        pleaseReport' "UNEXPECTED MODULE DECLARATION" "empty listing"
                , space
                , keyword "where"
                ]

        docs =
            formatModuleDocs (AST.Module.docs modu)

        importSpacer first second =
              case (first, second) of
                  (AST.Module.ImportComment _, AST.Module.ImportComment _) ->
                      []
                  (AST.Module.ImportComment _, _) ->
                      List.replicate 1 blankLine
                  (_, AST.Module.ImportComment _) ->
                      List.replicate 2 blankLine
                  (_, _) ->
                      []

        imports =
              AST.Module.imports modu
                  |> intersperseMap importSpacer formatImport

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
            intersperseMap spacer formatDeclaration $
                AST.Module.body modu

        mapIf fn m a =
            case m of
                Just x ->
                    fn x a
                Nothing ->
                    a
    in
        moduleLine
            |> mapIf (\x -> andThen [ blankLine, x ]) docs
            |> (if null imports then id else andThen imports . andThen [blankLine])
            |> andThen [ blankLine, blankLine ]
            |> andThen body


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
            stack1
                [ line $ row [ punc "{-|", space, literal first ]
                , line $ punc "-}"
                ]
        (first:rest) ->
            (line $ row [ punc "{-|", space, literal first ])
                |> andThen (map (line . literal) rest)
                |> andThen [ line $ punc "-}" ]


formatName :: MN.Raw -> Line
formatName name =
    identifier (List.intercalate "." name)


formatImport :: AST.Module.UserImport -> Box
formatImport aimport =
    case aimport of
        AST.Module.UserImport aimport' ->
            case RA.drop aimport' of
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
                                        pleaseReport "TODO" "multiline exposing"
                            Nothing ->
                                line $ row
                                    [ keyword "import"
                                    , space
                                    , as
                                    ]

        AST.Module.ImportComment c ->
            formatComment c


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
                    formatDefinition def

                AST.Declaration.Datatype name args ctors ->
                    let
                        ctor (tag,args') =
                            case allSingles $ map (formatType' ForCtor) args' of
                                Right args'' ->
                                    line $ row $ List.intersperse space $ (identifier tag):args''
                                Left [] ->
                                    line $ identifier tag
                                Left args'' ->
                                    stack1
                                        [ line $ identifier tag
                                        , stack1 args''
                                            |> indent
                                        ]
                    in
                        case ctors of
                            [] ->
                                pleaseReport "INVALID DATA TYPE" "no constructors"
                            (first:rest) ->
                                stack1
                                    [ line $ row
                                        [ keyword "type"
                                        , space
                                        , row $ List.intersperse space $ map identifier (name:args)
                                        ]
                                    , ctor first
                                        |> prefix (row [punc "=", space])
                                        |> andThen (map (prefix (row [punc "|", space]) . ctor) rest)
                                        |> indent
                                    ]

                AST.Declaration.TypeAlias name args typ ->
                    stack1
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

                AST.Declaration.PortAnnotation name typeComments typ ->
                    case
                        ( isLine $ formatCommented' typeComments formatType typ
                        , isLine $ formatCommented (line . identifier) name
                        )
                    of
                        (Right typ', Right name') ->
                            line $ row
                                [ keyword "port"
                                , space
                                , name'
                                , space
                                , punc ":"
                                , space
                                , typ'
                                ]
                        _ ->
                            pleaseReport "TODO" "multiline type in port annotation"

                AST.Declaration.PortDefinition name bodyComments expr ->
                    case isLine $ formatCommented (line . identifier) name of
                        Right name' ->
                            stack1
                                [ line $ row
                                    [ keyword "port"
                                    , space
                                    , name'
                                    , space
                                    , punc "="
                                    ]
                                , formatCommented' bodyComments formatExpression expr
                                    |> indent
                                ]
                        _ ->
                            pleaseReport "TODO" "multiline name in port definition"

                AST.Declaration.Fixity assoc precedenceComments precedence nameComments name ->
                    case
                        ( isLine $ (formatCommented' nameComments (line . formatInfixVar) name)
                        , isLine $ (formatCommented' precedenceComments (line . literal . show) precedence)
                        )
                    of
                        (Right name', Right precedence') ->
                            line $ row
                                [ case assoc of
                                      AST.Declaration.L -> keyword "infixl"
                                      AST.Declaration.R -> keyword "infixr"
                                      AST.Declaration.N -> keyword "infix"
                                , space
                                , precedence'
                                , space
                                , name'
                                ]
                        _ ->
                            pleaseReport "TODO" "multiline fixity declaration"


formatDefinition :: AST.Expression.Def -> Box
formatDefinition adef =
    case RA.drop adef of
        AST.Expression.Definition name args comments expr multiline ->
            let
              body =
                stack1 $ concat
                  [ map formatComment comments
                  , [ formatExpression expr ]
                  ]
            in
              case
                ( multiline --TODO: can this be removed?
                , isLine $ formatPattern True name
                , allSingles $ map (\(x,y) -> formatCommented' x (formatPattern True) y) args
                )
              of
                (_, Right name', Right args') ->
                    stack1 $
                        [ line $ row
                            [ row $ List.intersperse space $ (name':args')
                            , space
                            , punc "="
                            ]
                        , indent $ body
                        ]

                (_, Right name', Left args') ->
                    stack1
                      [ line $ name'
                      , indent $ stack1 $ concat
                          [ args'
                          , [ line $ punc "="
                            , body
                            ]
                          ]
                      ]

                _ ->
                    pleaseReport "UNEXPECTED TYPE ANNOTATION" "multiline name in let binding"

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
                    stack1
                        [ line $ row [ name', space, punc ":" ]
                        , typ'
                            |> indent
                        ]

                _ ->
                    pleaseReport "UNEXPECTED TYPE ANNOTATION" "multiline name"

        AST.Expression.LetComment comment ->
            formatComment comment


formatPattern :: Bool -> AST.Pattern.Pattern -> Box
formatPattern parensRequired apattern =
    case RA.drop apattern of
        AST.Pattern.Anything ->
            line $ keyword "_"

        AST.Pattern.UnitPattern comments ->
            formatUnit '(' ')' comments

        AST.Pattern.Literal lit ->
            formatLiteral lit

        AST.Pattern.Var var ->
            line $ formatVar var

        AST.Pattern.ConsPattern first rest final ->
            let
              first' = formatTailCommented (formatPattern True) first
              rest' = map (formatCommented (formatPattern False)) rest
              final' = formatHeadCommented (formatPattern False) final
            in
              formatBinary
                  False
                  first'
                  (map ((,) (line $ punc "::")) (rest'++[final']))
              |> if parensRequired then parens else id

        AST.Pattern.Data ctor [] ->
            if any ((==) '.') ctor then
                (line $ identifier ctor)
                    |> if parensRequired then parens else id
            else
                line $ identifier ctor

        AST.Pattern.Data ctor patterns ->
            elmApplication
                (line $ identifier ctor)
                (map (formatHeadCommented $ formatPattern True) patterns)
            |> if parensRequired then parens else id

        AST.Pattern.PatternParens pattern ->
            formatCommented (formatPattern False) pattern
              |> parens

        AST.Pattern.Tuple patterns ->
            elmGroup True "(" "," ")" False $ map (formatCommented $ formatPattern False) patterns

        AST.Pattern.EmptyListPattern comments ->
            formatUnit '[' ']' comments

        AST.Pattern.List patterns ->
            elmGroup True "[" "," "]" False $ map (formatCommented $ formatPattern False) patterns

        AST.Pattern.Record fields ->
            elmGroup True "{" "," "}" False $ map (formatCommented $ line . identifier) fields

        AST.Pattern.Alias pattern name ->
          case
            allSingles2
              (formatTailCommented (formatPattern True) pattern)
              (formatHeadCommented (line . identifier) name )
          of
            Right (pattern', name') ->
              line $ row
                [ pattern'
                , space
                , keyword "as"
                , space
                , name'
                ]

            Left (pattern', name') ->
              stack1
                [ pattern'
                , line $ keyword "as"
                , indent name'
                ]

          |> (if parensRequired then parens else id)


addSuffix :: Line -> Box -> Box
addSuffix suffix b =
    case destructure b of
        (l,[]) ->
            line $ row [ l, suffix ]
        (l1,ls) ->
            line l1
                |> andThen (map line $ init ls)
                |> andThen [ line $ row [ last ls, suffix ] ]


formatRecordPair :: Char -> (v -> Box) -> (Commented String, Commented v, Bool) -> Box
formatRecordPair delim formatValue (Commented pre k postK, v, multiline') =
    case
        isLine'
            (isLine $ formatCommented (line . identifier) $ Commented [] k postK)
            (force multiline' $ isLine $ formatCommented formatValue v)
    of
        Right (k', Right v') ->
            line $ row
                [ k'
                , space
                , delim'
                , space
                , v'
                ]
        Right (k', Left v') ->
            stack1
                [ line $ row [ k', space, delim' ]
                , indent v'
                ]
        Left (k', v') ->
            stack1
                [ k'
                , indent $ prefix (row [delim', space]) v'
                ]
    |> (\x -> Commented pre x []) |> formatCommented id
      where
        delim' =
          punc (delim : [])


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
                    stack1
                        [ line $ punc "["
                        , formatCommented formatExpression left
                            |> indent
                        , line $ punc ".."
                        , formatCommented formatExpression right
                            |> indent
                        , line $ punc "]"
                        ]

        AST.Expression.EmptyList comments ->
          formatUnit '[' ']' comments

        AST.Expression.ExplicitList exprs multiline ->
            elmGroup True "[" "," "]" multiline $ map (formatCommented formatExpression) exprs

        AST.Expression.Binops left ops multiline ->
            let
                formatPair ( po, o, pe, e ) =
                    ( formatCommented' po (line . formatInfixVar) o
                    , formatCommented' pe formatExpression e
                    )
            in
                formatBinary
                    multiline
                    (formatExpression left)
                    (map formatPair ops)

        AST.Expression.Lambda patterns bodyComments expr multiline ->
            case
                ( multiline
                , allSingles $ map (formatCommented (formatPattern True) . (\(c,p) -> Commented c p [])) patterns
                , bodyComments
                , isLine $ formatExpression expr
                )
            of
                (False, Right patterns', [], Right expr') ->
                    line $ row
                        [ punc "\\"
                        , row $ List.intersperse space $ patterns'
                        , space
                        , punc "->"
                        , space
                        , expr'
                        ]
                (_, Right patterns', _, _) ->
                    stack1
                        [ line $ row
                            [ punc "\\"
                            , row $ List.intersperse space $ patterns'
                            , space
                            , punc "->"
                            ]
                        , indent $ stack1 $
                            (map formatComment bodyComments)
                            ++ [ formatExpression expr ]
                        ]
                _ ->
                    pleaseReport "TODO" "multiline pattern in lambda"

        AST.Expression.Unary AST.Expression.Negative e ->
            prefix (punc "-") $ formatExpression e

        AST.Expression.App left args multiline ->
            case
                ( multiline
                , isLine $ formatExpression left
                , allSingles $ map (\(x,y) -> formatCommented' x formatExpression y) args
                )
            of
                (False, Right left', Right args') ->
                  line $ row
                      $ List.intersperse space $ (left':args')
                _ ->
                    formatExpression left
                        |> andThen (map (\(x,y) -> indent $ formatCommented' x formatExpression y) args)

        AST.Expression.If [] _ els ->
            stack1
                [ pleaseReport "INVALID IF EXPRESSION" "no if branch"
                , formatExpression els
                    |> indent
                ]
        AST.Expression.If (if':elseifs) elsComments els ->
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
                            stack1
                                [ line $ keyword key
                                , cond |> indent
                                , line $ keyword "then"
                                ]

                clause key (cond, multiline, bodyComments, body) =
                    stack1
                        [ opening key multiline $ formatExpression cond
                        , indent $ stack1 $
                            (map formatComment bodyComments)
                            ++ [ formatExpression body ]
                        ]
            in
                clause "if" if'
                    |> andThen (map (clause "else if") elseifs)
                    |> andThen
                        [ line $ keyword "else"
                        , indent $ stack1 $
                            (map formatComment elsComments)
                            ++ [ formatExpression els ]
                        ]

        AST.Expression.Let defs bodyComments expr ->
            let
                spacer first _ =
                    case isDefinition $ RA.drop first of
                        True ->
                            [ blankLine ]
                        False ->
                            []
            in
                (line $ keyword "let")
                    |> andThen
                        (defs
                            |> intersperseMap spacer formatDefinition
                            |> map indent
                        )
                    |> andThen
                        [ line $ keyword "in"
                        , indent $ stack1 $
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
                          stack1
                              [ line $ keyword "case"
                              , formatExpression subject
                                  |> indent
                              , line $ keyword "of"
                              ]

                clause (patternComments, pat, bodyComments, expr) =
                    case isLine $ formatPattern False pat of
                        Right pat' ->
                            stack1 $
                                (map formatComment patternComments)
                                ++
                                [ line $ row [ pat', space, keyword "->"]
                                , indent $ stack1 $
                                    (map formatComment bodyComments)
                                    ++ [ formatExpression expr ]
                                ]
                        _ ->
                            pleaseReport "TODO" "multiline case pattern"
            in
                opening
                    |> andThen
                        (clauses
                            |> map clause
                            |> List.intersperse blankLine
                            |> map indent
                        )

        AST.Expression.Tuple exprs multiline ->
            elmGroup True "(" "," ")" multiline $ map (formatCommented formatExpression) exprs

        AST.Expression.TupleFunction n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"

        AST.Expression.Access expr field ->
            formatExpression expr -- TODO: needs to have parens in some cases
                |> addSuffix (row $ [punc ".", identifier field])

        AST.Expression.AccessFunction field ->
            line $ identifier $ "." ++ field

        AST.Expression.RecordUpdate _ [] _ ->
          pleaseReport "INVALID RECORD UPDATE" "no fields"

        AST.Expression.RecordUpdate base (first:rest) multiline ->
          elmExtensionGroup
            multiline
            (formatCommented formatExpression base)
            (formatRecordPair '=' formatExpression first)
            (map (formatRecordPair '=' formatExpression) rest)

        AST.Expression.Record pairs' multiline ->
          elmGroup True "{" "," "}" multiline $ map (formatRecordPair '=' formatExpression) pairs'

        AST.Expression.EmptyRecord [] ->
            line $ punc "{}"

        AST.Expression.EmptyRecord comments ->
            case isLine $ stack1 $ map formatComment comments of
                Right comments' ->
                    line $ row [ punc "{", comments', punc "}" ]

                Left comments' ->
                    comments'

        AST.Expression.Parens expr ->
            parens $ formatCommented formatExpression expr

        AST.Expression.Unit comments ->
            formatUnit '(' ')' comments

        AST.Expression.GLShader _ _ _ ->
            pleaseReport "TODO" "glshader"


formatUnit :: Char -> Char -> Comments -> Box
formatUnit left right comments =
  case (left, comments) of
    (_, []) ->
      line $ punc (left : right : [])

    ('{', (LineComment _):_) ->
      surround left right $ prefix space $ stack1 $ map formatComment comments

    _ ->
      surround left right $ stack1 $ map formatComment comments


formatCommented :: (a -> Box) -> Commented a -> Box
formatCommented format (Commented pre inner post) =
    case
        ( allSingles $ fmap formatComment pre
        , allSingles $ fmap formatComment post
        , isLine $ format inner
        )
    of
        ( Right pre', Right post', Right inner' ) ->
            line $ row $ List.intersperse space $ concat [pre', [inner'], post']
        _ -> -- TODO: not tested
            stack1 $
                (map formatComment pre)
                ++ [ format inner ]
                ++ ( map formatComment post)


formatHeadCommented :: (a -> Box) -> (Comments, a) -> Box
formatHeadCommented format (pre, inner) =
    formatCommented' pre format inner


formatCommented' :: Comments -> (a -> Box) -> a -> Box
formatCommented' pre format inner =
    formatCommented format (Commented pre inner [])


formatTailCommented :: (a -> Box) -> (a, Comments) -> Box
formatTailCommented format (inner, post) =
  formatCommented format (Commented [] inner post)


formatComment :: Comment -> Box
formatComment comment =
    case comment of
        BlockComment c ->
            case c of
                [] ->
                    line $ punc "{- -}"
                (l:[]) ->
                    line $ row
                        [ punc "{-"
                        , space
                        , literal l
                        , space
                        , punc "-}"
                        ]
                (l1:ls) -> -- TODO: not tested
                    stack1
                        [ line $ row
                            [ punc "{-"
                            , space
                            , literal l1
                            ]
                        , stack1 $ map (line . literal) ls
                        , line $ punc "-}"
                        ]
        LineComment c ->
            mustBreak $ row [ punc "--", literal c ]


formatLiteral :: Literal -> Box
formatLiteral lit =
    case lit of
        IntNum i ->
            line $ literal $ show i
        FloatNum f ->
            line $ literal $ show f
        Chr c ->
            formatString SChar [c]
        Str s multi ->
            formatString (if multi then SMulti else SString) s
        Boolean True ->
            line $ literal "True"
        Boolean False ->
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


formatType :: Type -> Box
formatType =
    formatType' NotRequired


commaSpace :: Line
commaSpace =
    row
        [ punc ","
        , space
        ]


formatTypeConstructor :: TypeConstructor -> Box
formatTypeConstructor ctor =
    case ctor of
        NamedConstructor name ->
            line $ identifier name

        TupleConstructor n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"


formatType' :: TypeParensRequired -> Type -> Box
formatType' requireParens atype =
    case RA.drop atype of
        UnitType comments ->
          formatUnit '(' ')' comments

        FunctionType first rest final ->
            case
              allSingles $
                concat
                  [ [formatTailCommented (formatType' ForLambda) first]
                  , map (formatCommented $ formatType' ForLambda) rest
                  , [formatHeadCommented (formatType' ForLambda) final]
                  ]
            of
                Right typs ->
                  line $ row $ List.intersperse (row [ space, keyword "->", space]) typs
                Left [] ->
                  pleaseReport "INVALID FUNCTION TYPE" "no terms"
                Left (first':rest') ->
                  first'
                    |> andThen (rest' |> map (prefix $ row [keyword "->", space]))

            |> (if requireParens /= NotRequired then parens else id)

        TypeVariable var ->
            line $ identifier var

        TypeConstruction ctor args ->
            elmApplication
                (formatTypeConstructor ctor)
                (map (formatHeadCommented $ formatType' ForCtor) args)
                |> (if requireParens == ForCtor then parens else id)

        TypeParens type' ->
          parens $ formatCommented formatType type'

        TupleType types ->
          elmGroup True "(" "," ")" False (map (formatCommented formatType) types)

        EmptyRecordType comments ->
          formatUnit '{' '}' comments

        RecordType fields multiline ->
          elmGroup True "{" "," "}" multiline (map (formatRecordPair ':' formatType) fields)

        RecordExtensionType _ [] _ ->
          pleaseReport "INVALID RECORD TYPE EXTENSION" "no fields"

        RecordExtensionType ext (first:rest) multiline ->
          elmExtensionGroup
            multiline
            (formatCommented (line . identifier) ext)
            (formatRecordPair ':' formatType first)
            (map (formatRecordPair ':' formatType) rest)


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
            pleaseReport' "INVALID INFIX OPERATOR" "wildcard used as infix"
