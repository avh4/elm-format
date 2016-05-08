{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Box where

import Elm.Utils ((|>))
import Box
import Data.Version (showVersion)
import ElmVersion (ElmVersion(..))

import AST.V0_16
import qualified AST.Declaration
import qualified AST.Expression
import qualified AST.Module
import qualified AST.Module.Name as MN
import qualified AST.Pattern
import qualified AST.Variable
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified Paths_elm_format as This
import qualified Reporting.Annotation as RA
import qualified Text.Regex.Applicative as Regex
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
    case b of
      SingleLine b' ->
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
        , left
        , allSingles $ map fst ops
        , allSingles $ map snd ops
        )
    of
        ([], _, _, _, _) ->
            pleaseReport "INVALID BINARY EXPRESSION" "no operators"

        (_, False, SingleLine left', Right ops'', Right exprs') ->
            zip ops'' exprs'
                |> map (\(op,e) -> row [op, space, e])
                |> List.intersperse space
                |> (:) space
                |> (:) left'
                |> row
                |> line

        (_, _, left', Right ops'', _) ->
            zip ops'' (map snd ops)
                |> map (\(op,e) -> prefix (row [op, space]) e)
                |> stack1
                |> (\body -> stack1 [left', indent body])

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


data DeclarationType
  = DComment
  | DDefinition (Maybe AST.Variable.Ref)
  deriving (Show)


declarationType :: AST.Declaration.Decl -> DeclarationType
declarationType decl =
  case decl of
    AST.Declaration.Decl adecl ->
      case RA.drop adecl of
        AST.Declaration.Definition pat _ _ _ ->
          case RA.drop pat of
            AST.Pattern.Var name ->
              DDefinition $ Just name

            _ ->
              DDefinition Nothing

        AST.Declaration.Datatype (Commented _ (name, _) _) _ _ ->
          DDefinition $ Just $ AST.Variable.VarRef name

        AST.Declaration.TypeAlias _ (Commented _ (name, _) _) _ ->
          DDefinition $ Just $ AST.Variable.VarRef name

        AST.Declaration.PortDefinition (Commented _ name _) _ _ ->
          DDefinition $ Just $ AST.Variable.VarRef name

        AST.Declaration.TypeAnnotation (name, _) _ ->
          DDefinition $ Just name

        AST.Declaration.PortAnnotation (Commented _ name _) _ _ ->
          DDefinition $ Just $ AST.Variable.VarRef name

        AST.Declaration.Fixity _ _ _ _ name ->
          DDefinition $ Just name

    AST.Declaration.DocComment _ ->
      DDefinition Nothing

    AST.Declaration.BodyComment _ ->
      DComment


formatModuleHeader :: ElmVersion -> AST.Module.Module -> Box
formatModuleHeader elmVersion modu =
  let
      header =
        AST.Module.header modu

      moduleLine =
        case elmVersion of
          Elm_0_16 ->
            formatModuleLine_0_16 header

          Elm_0_17 ->
            formatModuleLine_0_17 header

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


formatModuleLine_0_16 :: AST.Module.Header -> Box
formatModuleLine_0_16 header =
  let
    formatExports =
      case formatListing formatVarValue $ AST.Module.exports header of
          Just listing ->
            listing
          _ ->
              line $ pleaseReport' "UNEXPECTED MODULE DECLARATION" "empty listing"

    whereClause =
      formatHeadCommented (line . keyword) (AST.Module.preExportsComments header, "where")
  in
    case
      ( formatCommented (line . formatName) $ AST.Module.name header
      , formatExports
      , whereClause
      )
    of
      (SingleLine name', SingleLine exports', SingleLine where') ->
        line $ row
          [ keyword "module"
          , space
          , name'
          , row [ space, exports' ]
          , space
          , where'
          ]
      (name', exports', _) ->
        stack1
          [ line $ keyword "module"
          , indent $ name'
          , indent $ exports'
          , indent $ whereClause
          ]


formatModuleLine_0_17 :: AST.Module.Header -> Box
formatModuleLine_0_17 header =
  let
    moduleKeyword =
      case AST.Module.moduleType header of
        AST.Module.UserModule ->
          keyword "module"
        AST.Module.PortModule ->
          row [ keyword "port", space, keyword "module" ]
        AST.Module.EffectModule ->
          row [ keyword "effect", space, keyword "module" ]

    exports =
      case formatListing formatVarValue $ AST.Module.exports header of
          Just listing ->
            listing
          _ ->
              line $ pleaseReport' "UNEXPECTED MODULE DECLARATION" "empty listing"

    whereClause =
      AST.Module.moduleSettings header
        |> fmap (map (\(k, v) -> ElmStructure.equalsPair (formatCommented (line . identifier) k) (formatCommented (line . identifier) v)))
        |> fmap (ElmStructure.group True "{" "," "}" False)
        |> fmap (\x -> [x])
        |> fmap (ElmStructure.spaceSepOrIndented (line $ keyword "where"))
        |> fmap (\x -> [x])
        |> Maybe.fromMaybe []

    exposingClause =
      case
        ( formatTailCommented (line . keyword) ("exposing", AST.Module.preExportsComments header)
        , exports
        )
      of
        (SingleLine exposing, SingleLine exports') ->
          line $ row [ exposing, space, exports' ]

        (exposing, exports') ->
          stack1
            [ exposing
            , indent exports'
            ]

    nameClause =
      case
        ( formatCommented (line . formatName) $ AST.Module.name header
        )
      of
        SingleLine name' ->
          line $ row
            [ moduleKeyword
            , space
            , name'
            ]

        name' ->
          stack1
            [ line $ moduleKeyword
            , indent $ name'
            ]
  in
    ElmStructure.spaceSepOrIndented
      nameClause
      (whereClause ++ [exposingClause])


formatModule :: ElmVersion -> AST.Module.Module -> Box
formatModule elmVersion modu =
    let
        spacer first second =
          case (declarationType first, declarationType second) of
            (DComment, DComment) ->
              []
            (_, DComment) ->
              List.replicate 3 blankLine
            (DComment, _) ->
              List.replicate 2 blankLine
            (DDefinition Nothing, DDefinition (Just _)) ->
              []
            (DDefinition a, DDefinition b) ->
              if a == b then
                []
              else
                List.replicate 2 blankLine

        body =
            intersperseMap spacer formatDeclaration $
                AST.Module.body modu


        initialComments' =
          case AST.Module.initialComments modu of
            [] ->
              []
            comments ->
              (map formatComment comments)
                ++ [ blankLine, blankLine ]
    in
      stack1 $
        initialComments'
          ++ (formatModuleHeader elmVersion modu)
          : body


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
                          (AST.Module.alias method)
                            |> fmap (formatImportClause
                            (Just . line . identifier)
                            "as")
                            |> Monad.join

                        (exposingPreKeyword, exposingPostKeywordAndListing)
                          = AST.Module.exposedVars method

                        exposing =
                          formatImportClause
                            (formatListing formatVarValue)
                            "exposing"
                            (exposingPreKeyword, exposingPostKeywordAndListing)

                        formatImportClause :: (a -> Maybe Box) -> String -> (Comments, (Comments, a)) -> Maybe Box
                        formatImportClause format keyw input =
                          case
                            fmap (fmap format) $ input
                          of
                            ([], ([], Nothing)) ->
                              Nothing

                            (preKeyword, (postKeyword, Just listing')) ->
                              case
                                ( formatHeadCommented (line . keyword) (preKeyword, keyw)
                                , formatHeadCommented id (postKeyword, listing')
                                )
                              of
                                (SingleLine keyword', SingleLine listing'') ->
                                  Just $ line $ row
                                    [ keyword'
                                    , space
                                    , listing''
                                    ]

                                (keyword', listing'') ->
                                  Just $ stack1
                                    [ keyword'
                                    , indent listing''
                                    ]

                            _ ->
                              Just $ pleaseReport "UNEXPECTED IMPORT" "import clause comments with no clause"
                    in
                        case
                          ( formatHeadCommented (line . formatName) name
                          , as
                          , exposing
                          )
                        of
                          ( SingleLine name', Just (SingleLine as'), Just (SingleLine exposing') ) ->
                            line $ row
                              [ keyword "import"
                              , space
                              , name'
                              , space
                              , as'
                              , space
                              , exposing'
                              ]

                          (SingleLine name', Just (SingleLine as'), Nothing) ->
                            line $ row
                              [ keyword "import"
                              , space
                              , name'
                              , space
                              , as'
                              ]

                          (SingleLine name', Nothing, Just (SingleLine exposing')) ->
                            line $ row
                              [ keyword "import"
                              , space
                              , name'
                              , space
                              , exposing'
                              ]

                          (SingleLine name', Nothing, Nothing) ->
                            line $ row
                              [ keyword "import"
                              , space
                              , name'
                              ]

                          ( SingleLine name', Just (SingleLine as'), Just exposing' ) ->
                            stack1
                              [ line (keyword "import")
                              , line $ row
                                [ name'
                                , space
                                , as'
                                ]
                              , indent exposing'
                              ]

                          ( SingleLine name', Just as', Just exposing' ) ->
                            stack1
                              [ line $ row
                                [ keyword "import"
                                , space
                                , name'
                                ]
                              , indent as'
                              , indent exposing'
                              ]

                          ( SingleLine name', Nothing, Just exposing' ) ->
                            stack1
                              [ line $ row
                                [ keyword "import"
                                , space
                                , name'
                                ]
                              , indent exposing'
                              ]

                          ( name', Just as', Just exposing' ) ->
                            stack1
                              [ line $ keyword "import"
                              , indent name'
                              , indent $ indent as'
                              , indent $ indent exposing'
                              ]

                          ( name', Nothing, Just exposing' ) ->
                            stack1
                              [ line $ keyword "import"
                              , indent name'
                              , indent $ indent exposing'
                              ]

                          ( name', Just as', Nothing ) ->
                            stack1
                              [ line $ keyword "import"
                              , indent name'
                              , indent $ indent as'
                              ]

                          ( name', Nothing, Nothing ) ->
                            stack1
                              [ line $ keyword "import"
                              , indent name'
                              ]


        AST.Module.ImportComment c ->
            formatComment c


formatListing :: (a -> Box) -> AST.Variable.Listing a -> Maybe Box
formatListing format listing =
    case listing of
        AST.Variable.ClosedListing ->
            Nothing

        AST.Variable.OpenListing comments ->
            Just $ parens $ formatCommented (line . keyword) $ fmap (const "..") comments

        AST.Variable.ExplicitListing vars multiline ->
            Just $ ElmStructure.group False "(" "," ")" multiline $ map (formatCommented format) vars


formatVarValue :: AST.Variable.Value -> Box
formatVarValue aval =
    case aval of
        AST.Variable.Value val ->
            line $ formatVar val

        AST.Variable.Alias name ->
            line $ identifier name

        AST.Variable.Union name listing ->
            case
              ( formatListing (line . identifier) listing
              , formatTailCommented (line . identifier) name
              , snd name
              )
            of
                (Just (SingleLine listing'), SingleLine name', []) ->
                    line $ row
                        [ name'
                        , listing'
                        ]
                (Just (SingleLine listing'), SingleLine name', _) ->
                    line $ row
                        [ name'
                        , space
                        , listing'
                        ]
                (Just listing', name', _) ->
                  stack1
                    [ name'
                    , indent $ listing'
                    ]
                (Nothing, name', _) ->
                    name'


formatDeclaration :: AST.Declaration.Decl -> Box
formatDeclaration decl =
    case decl of
        AST.Declaration.DocComment docs ->
            formatDocComment docs

        AST.Declaration.BodyComment c ->
            formatComment c

        AST.Declaration.Decl adecl ->
            case RA.drop adecl of
                AST.Declaration.Definition name args comments expr ->
                    formatDefinition name args comments expr

                AST.Declaration.TypeAnnotation name typ ->
                    formatTypeAnnotation name typ

                AST.Declaration.Datatype nameWithArgs middle final ->
                    let
                        ctor (tag,args') =
                            case allSingles $ map (formatHeadCommented $ formatType' ForCtor) args' of
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
                        case
                          (map (formatCommented ctor) middle) ++ [formatHeadCommented ctor final]
                        of
                          [] -> error "List can't be empty"
                          first:rest ->
                              case formatCommented formatNameWithArgs nameWithArgs of
                                SingleLine nameWithArgs' ->
                                  stack1
                                    [ line $ row
                                        [ keyword "type"
                                        , space
                                        , nameWithArgs'
                                        ]
                                    , first
                                        |> prefix (row [punc "=", space])
                                        |> andThen (map (prefix (row [punc "|", space])) rest)
                                        |> indent
                                    ]
                                nameWithArgs' ->
                                  stack1
                                    [ line $ keyword "type"
                                    , indent $ nameWithArgs'
                                    , first
                                        |> prefix (row [punc "=", space])
                                        |> andThen (map (prefix (row [punc "|", space])) rest)
                                        |> indent
                                    ]

                AST.Declaration.TypeAlias preAlias nameWithArgs typ ->
                  stack1
                    [ case
                        ( formatHeadCommented (line . keyword) (preAlias, "alias")
                        , formatCommented formatNameWithArgs nameWithArgs
                        )
                      of
                      (SingleLine alias, SingleLine nameWithArgs') ->
                        line $ row
                          [ keyword "type"
                          , space
                          , alias
                          , space
                          , nameWithArgs'
                          , space
                          , punc "="
                          ]
                      (SingleLine alias, nameWithArgs') ->
                        stack1
                          [ line $ row [keyword "type", space, alias]
                          , indent $ nameWithArgs'
                          , indent $ line $ punc "="
                          ]
                      (alias, nameWithArgs') ->
                        stack1
                          [ line $ keyword "type"
                          , indent $ alias
                          , indent $ nameWithArgs'
                          , indent $ line $ punc "="
                          ]
                    , formatHeadCommentedStack formatType typ
                        |> indent
                    ]


                AST.Declaration.PortAnnotation name typeComments typ ->
                    case
                        ( formatCommented' typeComments formatType typ
                        , formatCommented (line . identifier) name
                        )
                    of
                        (SingleLine typ', SingleLine name') ->
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
                    case formatCommented (line . identifier) name of
                        SingleLine name' ->
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
                        ( formatCommented' nameComments (line . formatInfixVar) name
                        , formatCommented' precedenceComments (line . literal . show) precedence
                        )
                    of
                        (SingleLine name', SingleLine precedence') ->
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


formatNameWithArgs :: (String, [(Comments, String)]) -> Box
formatNameWithArgs (name, args) =
  case allSingles $ map (formatHeadCommented (line . identifier)) args of
    Right args' ->
      line $ row $ List.intersperse space $ ((identifier name):args')
    Left args' ->
      stack1 $
        [ line $ identifier name ]
        ++ (map indent args')


formatDefinition :: AST.Pattern.Pattern
                      -> [(Comments, AST.Pattern.Pattern)]
                      -> [Comment]
                      -> AST.Expression.Expr
                      -> Box
formatDefinition name args comments expr =
  let
    body =
      stack1 $ concat
        [ map formatComment comments
        , [ formatExpression expr ]
        ]
  in
    case
      ( formatPattern True name
      , allSingles $ map (\(x,y) -> formatCommented' x (formatPattern True) y) args
      )
    of
      (SingleLine name', Right args') ->
          stack1 $
              [ line $ row
                  [ row $ List.intersperse space $ (name':args')
                  , space
                  , punc "="
                  ]
              , indent $ body
              ]

      (SingleLine name', Left args') ->
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
          pleaseReport "TODO" "multiline name in let binding"


formatTypeAnnotation :: (AST.Variable.Ref, Comments) -> (Comments, Type) -> Box
formatTypeAnnotation name typ =
  case
      ( formatTailCommented (line . formatVar) name
      , formatHeadCommented formatType typ
      )
  of
      (SingleLine name', SingleLine typ') ->
          line $ row
              [ name'
              , space
              , punc ":"
              , space
              , typ'
              ]

      (SingleLine name', typ') ->
          stack1
              [ line $ row [ name', space, punc ":" ]
              , typ'
                  |> indent
              ]

      (name', typ') ->
        stack1
          [ name'
          , indent $ line $ punc ":"
          , indent $ typ'
          ]


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
              rest' = map (formatCommented (formatPattern True)) rest
              final' = formatHeadCommented (formatPattern True) final
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
            ElmStructure.application
                False
                (line $ identifier ctor)
                (map (formatHeadCommented $ formatPattern True) patterns)
            |> if parensRequired then parens else id

        AST.Pattern.PatternParens pattern ->
            formatCommented (formatPattern False) pattern
              |> parens

        AST.Pattern.Tuple patterns ->
            ElmStructure.group True "(" "," ")" False $ map (formatCommented $ formatPattern False) patterns

        AST.Pattern.EmptyListPattern comments ->
            formatUnit '[' ']' comments

        AST.Pattern.List patterns ->
            ElmStructure.group True "[" "," "]" False $ map (formatCommented $ formatPattern False) patterns

        AST.Pattern.Record fields ->
            ElmStructure.group True "{" "," "}" False $ map (formatCommented $ line . identifier) fields

        AST.Pattern.Alias pattern name ->
          case
            ( formatTailCommented (formatPattern True) pattern
            , formatHeadCommented (line . identifier) name
            )
          of
            (SingleLine pattern', SingleLine name') ->
              line $ row
                [ pattern'
                , space
                , keyword "as"
                , space
                , name'
                ]

            (pattern', name') ->
              stack1
                [ pattern'
                , line $ keyword "as"
                , indent name'
                ]

          |> (if parensRequired then parens else id)


formatRecordPair :: Char -> (v -> Box) -> (Commented String, Commented v, Bool) -> Box
formatRecordPair delim formatValue (Commented pre k postK, v, multiline') =
    case
      ( formatCommented (line . identifier) $ Commented [] k postK
      , multiline'
      , formatCommented formatValue v
      )
    of
      (SingleLine k', False, SingleLine v') ->
            line $ row
                [ k'
                , space
                , delim'
                , space
                , v'
                ]
      (SingleLine k', _, v') ->
            stack1
                [ line $ row [ k', space, delim' ]
                , indent v'
                ]
      (k', _, v') ->
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
                , formatCommented formatExpression left
                , formatCommented formatExpression right
                )
            of
                (False, SingleLine left', SingleLine right') ->
                    line $ row
                        [ punc "["
                        , left'
                        , punc ".."
                        , right'
                        , punc "]"
                        ]
                (_, left', right') ->
                    stack1
                        [ line $ punc "["
                        , indent left'
                        , line $ punc ".."
                        , indent right'
                        , line $ punc "]"
                        ]

        AST.Expression.EmptyList comments ->
          formatUnit '[' ']' comments

        AST.Expression.ExplicitList exprs multiline ->
            ElmStructure.group True "[" "," "]" multiline $ map (formatCommented formatExpression) exprs

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
                , formatExpression expr
                )
            of
                (False, Right patterns', [], SingleLine expr') ->
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
          ElmStructure.application
            multiline
            (formatExpression left)
            (map (\(x,y) -> formatCommented' x formatExpression y) args)

        AST.Expression.If if' elseifs (elsComments, els) ->
            let
                opening key cond =
                    case (key, cond) of
                        (SingleLine key', SingleLine cond') ->
                            line $ row
                                [ key'
                                , space
                                , cond'
                                , space
                                , keyword "then"
                                ]
                        _ ->
                            stack1
                                [ key
                                , cond |> indent
                                , line $ keyword "then"
                                ]

                formatIf (cond, body) =
                    stack1
                        [ opening (line $ keyword "if") $ formatCommented formatExpression cond
                        , indent $ formatCommented_ True formatExpression body
                        ]

                formatElseIf (ifComments, (cond, body)) =
                  let
                    key =
                      case (formatHeadCommented id (ifComments, line $ keyword "if")) of
                        SingleLine key' ->
                          line $ row [ keyword "else", space, key' ]
                        key' ->
                          stack1
                            [ line $ keyword "else"
                            , key'
                            ]
                  in
                    stack1
                      [ opening key $ formatCommented formatExpression cond
                      , indent $ formatCommented_ True formatExpression body
                      ]
            in
                formatIf if'
                    |> andThen (map formatElseIf elseifs)
                    |> andThen
                        [ line $ keyword "else"
                        , indent $ formatCommented_ True formatExpression (Commented elsComments els [])
                        ]

        AST.Expression.Let defs bodyComments expr ->
            let
                spacer first _ =
                    case first of
                        AST.Expression.LetDefinition _ _ _ _ ->
                            [ blankLine ]
                        _ ->
                            []

                formatDefinition' def =
                  case def of
                    AST.Expression.LetDefinition name args comments expr' ->
                      formatDefinition name args comments expr'

                    AST.Expression.LetAnnotation name typ ->
                      formatTypeAnnotation name typ

                    AST.Expression.LetComment comment ->
                        formatComment comment
            in
                (line $ keyword "let")
                    |> andThen
                        (defs
                            |> intersperseMap spacer formatDefinition'
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
                    , formatCommented formatExpression subject
                    )
                  of
                      (False, SingleLine subject') ->
                          line $ row
                              [ keyword "case"
                              , space
                              , subject'
                              , space
                              , keyword "of"
                              ]
                      (_, subject') ->
                          stack1
                              [ line $ keyword "case"
                              , indent subject'
                              , line $ keyword "of"
                              ]

                clause (pat, expr) =
                    case
                      ( pat
                      , formatPattern False $ (\(Commented _ x _) -> x) pat
                      , formatCommentedStack (formatPattern False) pat
                      , formatHeadCommentedStack formatExpression expr
                      )
                    of
                        (_, _, SingleLine pat', body') ->
                            stack1
                                [ line $ row [ pat', space, keyword "->"]
                                , indent body'
                                ]
                        (Commented pre _ [], SingleLine pat', _, body') ->
                            stack1 $
                                (map formatComment pre)
                                ++ [ line $ row [ pat', space, keyword "->"]
                                   , indent body'
                                   ]
                        (_, _, pat', body') ->
                            stack1 $
                              [ pat'
                              , line $ keyword "->"
                              , indent body'
                              ]
            in
                opening
                    |> andThen
                        (clauses
                            |> map clause
                            |> List.intersperse blankLine
                            |> map indent
                        )

        AST.Expression.Tuple exprs multiline ->
            ElmStructure.group True "(" "," ")" multiline $ map (formatCommented formatExpression) exprs

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
          ElmStructure.extensionGroup
            multiline
            (formatCommented formatExpression base)
            (formatRecordPair '=' formatExpression first)
            (map (formatRecordPair '=' formatExpression) rest)

        AST.Expression.Record pairs' multiline ->
          ElmStructure.group True "{" "," "}" multiline $ map (formatRecordPair '=' formatExpression) pairs'

        AST.Expression.EmptyRecord [] ->
            line $ punc "{}"

        AST.Expression.EmptyRecord comments ->
            case stack1 $ map formatComment comments of
                SingleLine comments' ->
                    line $ row [ punc "{", comments', punc "}" ]

                comments' ->
                    comments'

        AST.Expression.Parens expr ->
            parens $ formatCommented formatExpression expr

        AST.Expression.Unit comments ->
            formatUnit '(' ')' comments

        AST.Expression.GLShader src ->
          line $ row
            [ punc "[glsl|"
            , literal $ src
            , punc "|]"
            ]


formatUnit :: Char -> Char -> Comments -> Box
formatUnit left right comments =
  case (left, comments) of
    (_, []) ->
      line $ punc (left : right : [])

    ('{', (LineComment _):_) ->
      surround left right $ prefix space $ stack1 $ map formatComment comments

    _ ->
      surround left right $
        case allSingles $ map formatComment comments of
          Right comments' ->
            line $ row $ List.intersperse space comments'

          Left comments' ->
            stack1 comments'


formatCommented_ :: Bool -> (a -> Box) -> Commented a -> Box
formatCommented_ forceMultiline format (Commented pre inner post) =
    case
        ( forceMultiline
        , allSingles $ fmap formatComment pre
        , allSingles $ fmap formatComment post
        , format inner
        )
    of
        ( False, Right pre', Right post', SingleLine inner' ) ->
            line $ row $ List.intersperse space $ concat [pre', [inner'], post']
        (_, _, _, inner') ->
            stack1 $
                (map formatComment pre)
                ++ [ inner' ]
                ++ ( map formatComment post)


formatCommented :: (a -> Box) -> Commented a -> Box
formatCommented =
  formatCommented_ False


formatHeadCommented :: (a -> Box) -> (Comments, a) -> Box
formatHeadCommented format (pre, inner) =
    formatCommented' pre format inner


formatCommented' :: Comments -> (a -> Box) -> a -> Box
formatCommented' pre format inner =
    formatCommented format (Commented pre inner [])


formatTailCommented :: (a -> Box) -> (a, Comments) -> Box
formatTailCommented format (inner, post) =
  formatCommented format (Commented [] inner post)


formatCommentedStack :: (a -> Box) -> Commented a -> Box
formatCommentedStack format (Commented pre inner post) =
  stack1 $
    (map formatComment pre)
      ++ [ format inner ]
      ++ (map formatComment post)


formatHeadCommentedStack :: (a -> Box) -> (Comments, a) -> Box
formatHeadCommentedStack format (pre, inner) =
  formatCommentedStack format (Commented pre inner [])


formatComment :: Comment -> Box
formatComment comment =
    case comment of
        BlockComment c ->
            case c of
                [] ->
                    line $ punc "{- -}"
                [l] ->
                    line $ row
                        [ punc "{-"
                        , space
                        , literal l
                        , space
                        , punc "-}"
                        ]
                ls ->
                    stack1
                        [ prefix
                            (row [ punc "{-", space ])
                            (stack1 $ map (line . literal) ls)
                        , line $ punc "-}"
                        ]
        LineComment c ->
            mustBreak $ row [ punc "--", literal c ]


formatLiteral :: Literal -> Box
formatLiteral lit =
    case lit of
        IntNum i DecimalInt ->
            line $ literal $ show i
        IntNum i HexadecimalInt ->
            line $ literal $
              if i <= 0xFF then
                printf "0x%02X" i
              else if i <= 0xFFFF then
                printf "0x%04X" i
              else if i <= 0xFFFFFFFF then
                printf "0x%08X" i
              else
                printf "0x%016X" i
        FloatNum f DecimalFloat ->
            line $ literal $ printf "%f" f
        FloatNum f ExponentFloat ->
            line $ literal $ printf "%e" f
        Chr c ->
            formatString SChar [c]
        Str s multi ->
            formatString (if multi then SMulti else SString) s
        Boolean b ->
            line $ literal $ show b


data StringStyle
    = SChar
    | SString
    | SMulti
    deriving (Eq)


formatString :: StringStyle -> String -> Box
formatString style s =
  case style of
      SChar ->
        stringBox "\'" id
      SString ->
        stringBox "\"" id
      SMulti ->
        stringBox "\"\"\"" escapeMultiQuote
  where
    stringBox quotes escaper =
      line $ row
          [ punc quotes
          , literal $ escaper $ concatMap fix s
          , punc quotes
          ]

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
        else if c == '\xA0' then
            [c] -- Workaround for https://github.com/elm-lang/elm-compiler/issues/1279
        else if Char.isSpace c then
            hex c
        else
            [c]

    hex char =
        "\\x" ++ (printf fmt $ Char.ord char)
      where
        fmt =
          if Char.ord char <= 0xFF then
            "%02X"
          else
            "%04X"

    escapeMultiQuote =
        let
            quote =
                Regex.sym '"'

            oneOrMoreQuotes =
                Regex.some quote

            escape =
                ("\\\"\\\"\\" ++) . (List.intersperse '\\')
        in
            Regex.replace $ escape <$> oneOrMoreQuotes <* quote <* quote


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

        FunctionType first rest final forceMultiline ->
            case
              ( forceMultiline
              , allSingles $
                  concat
                    [ [formatTailCommented (formatType' ForLambda) first]
                    , map (formatCommented $ formatType' ForLambda) rest
                    , [formatHeadCommented (formatType' ForLambda) final]
                    ]
              )
            of
                (False, Right typs) ->
                  line $ row $ List.intersperse (row [ space, keyword "->", space]) typs
                (True, Right []) ->
                  pleaseReport "INVALID FUNCTION TYPE" "no terms"
                (True, Right (first':rest')) ->
                  (line first')
                    |> andThen (rest' |> map line |> map (prefix $ row [keyword "->", space]))
                (_, Left []) ->
                  pleaseReport "INVALID FUNCTION TYPE" "no terms"
                (_, Left (first':rest')) ->
                  first'
                    |> andThen (rest' |> map (prefix $ row [keyword "->", space]))

            |> (if requireParens /= NotRequired then parens else id)

        TypeVariable var ->
            line $ identifier var

        TypeConstruction ctor args ->
            ElmStructure.application
                False
                (formatTypeConstructor ctor)
                (map (formatHeadCommented $ formatType' ForCtor) args)
                |> (if requireParens == ForCtor then parens else id)

        TypeParens type' ->
          parens $ formatCommented formatType type'

        TupleType types ->
          ElmStructure.group True "(" "," ")" False (map (formatCommented formatType) types)

        EmptyRecordType comments ->
          formatUnit '{' '}' comments

        RecordType fields multiline ->
          ElmStructure.group True "{" "," "}" multiline (map (formatRecordPair ':' formatType) fields)

        RecordExtensionType _ [] _ ->
          pleaseReport "INVALID RECORD TYPE EXTENSION" "no fields"

        RecordExtensionType ext (first:rest) multiline ->
          ElmStructure.extensionGroup
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


formatInfixVar :: AST.Variable.Ref -> Line
formatInfixVar var =
    case var of
        AST.Variable.VarRef name ->
            identifier $ "`" ++ name ++ "`"
        AST.Variable.OpRef name ->
            identifier name
