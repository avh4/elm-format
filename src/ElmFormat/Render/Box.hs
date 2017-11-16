{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Box where

import Elm.Utils ((|>))
import Box
import ElmVersion (ElmVersion(..))
import Defaults

import AST.V0_16
import qualified AST.Declaration
import qualified AST.Expression
import qualified AST.Module
import qualified AST.Pattern
import qualified AST.Variable
import qualified Cheapskate.Types as Markdown
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as Text
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified ElmFormat.Render.Markdown
import qualified ElmFormat.Version
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as RA
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result
import Text.Printf (printf)
import Util.List


pleaseReport' :: String -> String -> Line
pleaseReport' what details =
    keyword $ "<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"


pleaseReport :: String -> String -> Box
pleaseReport what details =
    line $ pleaseReport' what details


surround :: Int -> Char -> Char -> Box -> Box
surround tabSize left right b =
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
                  |> prefix tabSize left'
              , line $ right'
              ]


parens :: Int -> Box -> Box
parens tabSize =
    surround tabSize '(' ')'


formatBinary :: Int -> Bool -> Box -> [ ( Bool, Comments, Box, Box ) ] -> Box
formatBinary tabSize multiline left ops =
    case ops of
        [] ->
            left

        ( isLeftPipe, comments, op, next ) : rest ->
            if isLeftPipe then
                ElmStructure.forceableSpaceSepOrIndented multiline
                    (ElmStructure.spaceSepOrStack left $
                        concat
                            [ Maybe.maybeToList $ formatComments tabSize comments
                            , [op]
                            ]
                    )
                    [formatBinary tabSize multiline next rest]
            else
                formatBinary
                    tabSize
                    multiline
                    (ElmStructure.forceableSpaceSepOrIndented multiline left [formatCommented' tabSize comments id $ ElmStructure.spaceSepOrPrefix tabSize op next])
                    rest


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
  | DStarter
  | DCloser
  | DDefinition (Maybe AST.Variable.Ref)
  | DFixity
  | DDocComment
  deriving (Show)


declarationType :: AST.Declaration.Decl -> DeclarationType
declarationType decl =
  case decl of
    AST.Declaration.Decl adecl ->
      case RA.drop adecl of
        AST.Declaration.Definition pat _ _ _ ->
          case RA.drop pat of
            AST.Pattern.VarPattern name ->
              DDefinition $ Just $ AST.Variable.VarRef [] name

            AST.Pattern.OpPattern name ->
              DDefinition $ Just $ AST.Variable.OpRef name

            _ ->
              DDefinition Nothing

        AST.Declaration.Datatype (Commented _ (name, _) _) _ ->
          DDefinition $ Just $ AST.Variable.TagRef [] name

        AST.Declaration.TypeAlias _ (Commented _ (name, _) _) _ ->
          DDefinition $ Just $ AST.Variable.TagRef [] name

        AST.Declaration.PortDefinition (Commented _ name _) _ _ ->
          DDefinition $ Just $ AST.Variable.VarRef [] name

        AST.Declaration.TypeAnnotation (name, _) _ ->
          DDefinition $ Just name

        AST.Declaration.PortAnnotation (Commented _ name _) _ _ ->
          DDefinition $ Just $ AST.Variable.VarRef [] name

        AST.Declaration.Fixity _ _ _ _ _name ->
          DFixity

    AST.Declaration.DocComment _ ->
      DDocComment

    AST.Declaration.BodyComment CommentTrickOpener ->
      DStarter

    AST.Declaration.BodyComment CommentTrickCloser ->
      DCloser

    AST.Declaration.BodyComment _ ->
      DComment


formatModuleHeader :: ElmVersion -> Int -> AST.Module.Module -> Box
formatModuleHeader elmVersion tabSize modu =
  let
      header =
        AST.Module.header modu

      moduleLine =
        case elmVersion of
          Elm_0_16 ->
            formatModuleLine_0_16 tabSize header

          Elm_0_17 ->
            formatModuleLine elmVersion tabSize header

          Elm_0_18 ->
            formatModuleLine elmVersion tabSize header

          Elm_0_18_Upgrade ->
              formatModuleLine elmVersion tabSize header

      docs =
          fmap (formatModuleDocs elmVersion tabSize) $ RA.drop $ AST.Module.docs modu

      imports =
          formatImports elmVersion tabSize modu

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


formatImports :: ElmVersion -> Int -> AST.Module.Module -> [Box]
formatImports elmVersion tabSize modu =
    let
        (comments, imports) =
            AST.Module.imports modu
    in
    [ formatComments tabSize comments
        |> maybeToList
    , imports
        |> Map.assocs
        |> fmap (\(name, (pre, method)) -> formatImport elmVersion tabSize ((pre, name), method))
    ]
        |> List.filter (not . List.null)
        |> List.intersperse [blankLine]
        |> concat


formatModuleLine_0_16 :: Int -> AST.Module.Header -> Box
formatModuleLine_0_16 tabSize header =
  let
    elmVersion = Elm_0_16

    formatExports =
      case AST.Module.exports header of
        KeywordCommented _ _ value ->
          case formatListing tabSize (formatDetailedListing elmVersion tabSize) value of
            Just listing ->
              listing
            _ ->
                pleaseReport "UNEXPECTED MODULE DECLARATION" "empty listing"

    whereClause =
      case AST.Module.exports header of
        KeywordCommented pre post _ ->
          formatCommented tabSize (line . keyword) (Commented pre "where" post)
  in
    case
      ( formatCommented tabSize (line . formatQualifiedUppercaseIdentifier elmVersion) $ AST.Module.name header
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


formatModuleLine :: ElmVersion -> Int -> AST.Module.Header -> Box
formatModuleLine elmVersion tabSize header =
  let
    tag =
      case AST.Module.srcTag header of
        AST.Module.Normal ->
          line $ keyword "module"

        AST.Module.Port comments ->
          ElmStructure.spaceSepOrIndented
            (formatTailCommented tabSize (line . keyword) ("port", comments))
            [ line $ keyword "module" ]

        AST.Module.Effect comments ->
          ElmStructure.spaceSepOrIndented
            (formatTailCommented tabSize (line . keyword) ("effect", comments))
            [ line $ keyword "module" ]

    exports list =
      case formatListing tabSize (formatDetailedListing elmVersion tabSize) $ list of
          Just listing ->
            listing
          _ ->
              pleaseReport "UNEXPECTED MODULE DECLARATION" "empty listing"

    formatSetting (k, v) =
      formatRecordPair elmVersion tabSize "=" (line . formatUppercaseIdentifier elmVersion) (k, v, False)

    formatSettings settings =
      map formatSetting settings
        |> ElmStructure.group tabSize True "{" "," "}" False

    whereClause =
      AST.Module.moduleSettings header
        |> fmap (formatKeywordCommented tabSize "where" formatSettings)
        |> fmap (\x -> [x])
        |> Maybe.fromMaybe []

    exposingClause =
      formatKeywordCommented tabSize "exposing" exports $ AST.Module.exports header

    nameClause =
      case
        ( tag
        , formatCommented tabSize (line . formatQualifiedUppercaseIdentifier elmVersion) $ AST.Module.name header
        )
      of
        (SingleLine tag', SingleLine name') ->
          line $ row
            [ tag'
            , space
            , name'
            ]

        (tag', name') ->
          stack1
            [ tag'
            , indent $ name'
            ]
  in
    ElmStructure.spaceSepOrIndented
      nameClause
      (whereClause ++ [exposingClause])


formatModule :: ElmVersion -> Int -> AST.Module.Module -> Box
formatModule elmVersion tabSize modu =
    let
        initialComments' =
          case AST.Module.initialComments modu of
            [] ->
              []
            comments ->
              (map (formatComment tabSize) comments)
                ++ [ blankLine, blankLine ]

        spaceBeforeBody =
            case AST.Module.body modu of
                [] -> 0
                AST.Declaration.BodyComment _ : _ -> 3
                _ -> 2
    in
      stack1 $
          concat
              [ initialComments'
              , [ formatModuleHeader elmVersion tabSize modu ]
              , List.replicate spaceBeforeBody blankLine
              , maybeToList (formatModuleBody 2 elmVersion tabSize modu)
              ]


formatModuleBody :: Int -> ElmVersion -> Int -> AST.Module.Module -> Maybe Box
formatModuleBody linesBetween elmVersion tabSize modu =
    let
        spacer first second =
          case (declarationType first, declarationType second) of
            (DStarter, _) ->
              []
            (_, DCloser) ->
              []
            (DComment, DComment) ->
              []
            (_, DComment) ->
              List.replicate (linesBetween + 1) blankLine
            (DComment, _) ->
              List.replicate linesBetween blankLine
            (DDocComment, DDefinition _) ->
              []
            (DDefinition Nothing, DDefinition (Just _)) ->
              List.replicate linesBetween blankLine
            (DDefinition _, DStarter) ->
              List.replicate linesBetween blankLine
            (DDefinition a, DDefinition b) ->
              if a == b then
                []
              else
                List.replicate linesBetween blankLine
            (DCloser, _) ->
              List.replicate linesBetween blankLine
            (_, DDocComment) ->
              List.replicate linesBetween blankLine
            (DDocComment, DStarter) ->
              []
            (DFixity, DFixity) ->
              []
            (DFixity, _) ->
              List.replicate linesBetween blankLine
            (_, DFixity) ->
              List.replicate linesBetween blankLine

        boxes =
            intersperseMap spacer (formatDeclaration elmVersion tabSize) $
                AST.Module.body modu
    in
        case boxes of
            [] -> Nothing
            _ -> Just $ stack1 boxes


formatModuleDocs :: ElmVersion -> Int -> Markdown.Blocks -> Box
formatModuleDocs elmVersion tabSize blocks =
    let
        format :: AST.Module.Module -> String
        format modu =
            let
                box =
                    case
                        ( formatImports elmVersion tabSize modu
                        , formatModuleBody 1 elmVersion tabSize modu
                        )
                    of
                        ( [], Nothing ) -> Nothing
                        ( imports, Nothing ) -> Just $ stack1 imports
                        ( [], Just body) -> Just body
                        ( imports, Just body ) -> Just $ stack1 (imports ++ [blankLine, body])
            in
                box
                    |> fmap (Text.unpack . (Box.render tabSize))
                    |> fromMaybe ""

        reformat :: String -> Maybe String
        reformat source =
            source
                |> Parse.parseSource
                |> Result.toMaybe
                |> fmap format

        content :: String
        content =
            ElmFormat.Render.Markdown.formatMarkdown reformat blocks
    in
        formatDocComment content


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


formatImport :: ElmVersion -> Int -> AST.Module.UserImport -> Box
formatImport elmVersion tabSize (name, method) =
    let
        as =
          (AST.Module.alias method)
            |> fmap (formatImportClause
            (Just . line . formatUppercaseIdentifier elmVersion)
            "as")
            |> Monad.join

        (exposingPreKeyword, exposingPostKeywordAndListing)
          = AST.Module.exposedVars method

        exposing =
          formatImportClause
            (formatListing tabSize (formatDetailedListing elmVersion tabSize))
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
                ( formatHeadCommented tabSize (line . keyword) (preKeyword, keyw)
                , formatHeadCommented tabSize id (postKeyword, listing')
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
        ( formatHeadCommented tabSize (line . formatQualifiedUppercaseIdentifier elmVersion) name
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
            [ line $ row
              [ keyword "import"
              , space
              , name'
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


formatListing :: Int -> (a -> [Box]) -> AST.Variable.Listing a -> Maybe Box
formatListing tabSize format listing =
    case listing of
        AST.Variable.ClosedListing ->
            Nothing

        AST.Variable.OpenListing comments ->
            Just $ parens tabSize $ formatCommented tabSize (line . keyword) $ fmap (const "..") comments

        AST.Variable.ExplicitListing vars multiline ->
            Just $ ElmStructure.group tabSize False "(" "," ")" multiline $ format vars


formatDetailedListing :: ElmVersion -> Int -> AST.Module.DetailedListing -> [Box]
formatDetailedListing elmVersion tabSize listing =
    concat
        [ formatCommentedMap
            tabSize
            (\name () -> AST.Variable.OpValue name)
            (formatVarValue elmVersion tabSize)
            (AST.Module.operators listing)
        , formatCommentedMap
            tabSize
            (\name (inner, listing) -> AST.Variable.Union (name, inner) listing)
            (formatVarValue elmVersion tabSize)
            (AST.Module.types listing)
        , formatCommentedMap
            tabSize
            (\name () -> AST.Variable.Value name)
            (formatVarValue elmVersion tabSize)
            (AST.Module.values listing)
        ]


formatCommentedMap :: Int -> (k -> v -> a) -> (a -> Box) ->  AST.Variable.CommentedMap k v -> [Box]
formatCommentedMap tabSize construct format values =
    let
        format' (k, Commented pre v post)
            = formatCommented tabSize format $ Commented pre (construct k v) post
    in
    values
        |> Map.assocs
        |> map format'


formatVarValue :: ElmVersion -> Int -> AST.Variable.Value -> Box
formatVarValue elmVersion tabSize aval =
    case aval of
        AST.Variable.Value val ->
            line $ formatLowercaseIdentifier elmVersion [] val

        AST.Variable.OpValue (SymbolIdentifier name) ->
            line $ identifier $ "(" ++ name ++ ")"

        AST.Variable.Union name listing ->
            case
              ( formatListing
                  tabSize
                  (formatCommentedMap
                      tabSize
                      (\name () -> name)
                      (line . formatUppercaseIdentifier elmVersion)
                  )
                  listing
              , formatTailCommented tabSize (line . formatUppercaseIdentifier elmVersion) name
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


formatDeclaration :: ElmVersion -> Int -> AST.Declaration.Decl -> Box
formatDeclaration elmVersion tabSize decl =
    case decl of
        AST.Declaration.DocComment docs ->
            formatModuleDocs elmVersion tabSize docs

        AST.Declaration.BodyComment c ->
            formatComment tabSize c

        AST.Declaration.Decl adecl ->
            case RA.drop adecl of
                AST.Declaration.Definition name args comments expr ->
                    formatDefinition elmVersion tabSize name args comments expr

                AST.Declaration.TypeAnnotation name typ ->
                    formatTypeAnnotation elmVersion tabSize name typ

                AST.Declaration.Datatype nameWithArgs tags ->
                    let
                        ctor (tag,args') =
                            case allSingles $ map (formatHeadCommented tabSize $ formatType' elmVersion tabSize ForCtor) args' of
                                Right args'' ->
                                    line $ row $ List.intersperse space $ (formatUppercaseIdentifier elmVersion tag):args''
                                Left [] ->
                                    line $ formatUppercaseIdentifier elmVersion tag
                                Left args'' ->
                                    stack1
                                        [ line $ formatUppercaseIdentifier elmVersion tag
                                        , stack1 args''
                                            |> indent
                                        ]
                    in
                        case
                            formatOpenCommentedList tabSize ctor tags
                        of
                          [] -> error "List can't be empty"
                          first:rest ->
                              case formatCommented tabSize (formatNameWithArgs elmVersion tabSize) nameWithArgs of
                                SingleLine nameWithArgs' ->
                                  stack1
                                    [ line $ row
                                        [ keyword "type"
                                        , space
                                        , nameWithArgs'
                                        ]
                                    , first
                                        |> prefix tabSize (row [punc "=", space])
                                        |> andThen (map (prefix tabSize (row [punc "|", space])) rest)
                                        |> indent
                                    ]
                                nameWithArgs' ->
                                  stack1
                                    [ line $ keyword "type"
                                    , indent $ nameWithArgs'
                                    , first
                                        |> prefix tabSize (row [punc "=", space])
                                        |> andThen (map (prefix tabSize (row [punc "|", space])) rest)
                                        |> indent
                                    ]

                AST.Declaration.TypeAlias preAlias nameWithArgs typ ->
                  ElmStructure.definition "=" True
                    (line $ keyword "type")
                    [ formatHeadCommented tabSize (line . keyword) (preAlias, "alias")
                    , formatCommented tabSize (formatNameWithArgs elmVersion tabSize) nameWithArgs
                    ]
                    (formatHeadCommentedStack tabSize (formatType elmVersion tabSize) typ)

                AST.Declaration.PortAnnotation name typeComments typ ->
                  ElmStructure.definition ":" False
                    (line $ keyword "port")
                    [ formatCommented tabSize (line . formatLowercaseIdentifier elmVersion []) name ]
                    (formatCommented' tabSize typeComments (formatType elmVersion tabSize) typ)

                AST.Declaration.PortDefinition name bodyComments expr ->
                  ElmStructure.definition "=" True
                    (line $ keyword "port")
                    [formatCommented tabSize (line . formatLowercaseIdentifier elmVersion []) name]
                    (formatCommented' tabSize bodyComments (formatExpression elmVersion tabSize SyntaxSeparated) expr)

                AST.Declaration.Fixity assoc precedenceComments precedence nameComments name ->
                    case
                        ( formatCommented' tabSize nameComments (line . formatInfixVar elmVersion) name
                        , formatCommented' tabSize precedenceComments (line . literal . show) precedence
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


formatNameWithArgs :: ElmVersion -> Int -> (UppercaseIdentifier, [(Comments, LowercaseIdentifier)]) -> Box
formatNameWithArgs elmVersion tabSize (name, args) =
  case allSingles $ map (formatHeadCommented tabSize (line . formatLowercaseIdentifier elmVersion [])) args of
    Right args' ->
      line $ row $ List.intersperse space $ ((formatUppercaseIdentifier elmVersion name):args')
    Left args' ->
      stack1 $
        [ line $ formatUppercaseIdentifier elmVersion name ]
        ++ (map indent args')


formatDefinition :: ElmVersion -> Int
                      -> AST.Pattern.Pattern
                      -> [(Comments, AST.Pattern.Pattern)]
                      -> [Comment]
                      -> AST.Expression.Expr
                      -> Box
formatDefinition elmVersion tabSize name args comments expr =
  let
    body =
      stack1 $ concat
        [ map (formatComment tabSize) comments
        , [ formatExpression elmVersion tabSize SyntaxSeparated expr ]
        ]
  in
    ElmStructure.definition "=" True
      (formatPattern elmVersion tabSize True name)
      (map (\(x,y) -> formatCommented' tabSize x (formatPattern elmVersion tabSize True) y) args)
      body


formatTypeAnnotation :: ElmVersion -> Int -> (AST.Variable.Ref, Comments) -> (Comments, Type) -> Box
formatTypeAnnotation elmVersion tabSize name typ =
  ElmStructure.definition ":" False
    (formatTailCommented tabSize (line . formatVar elmVersion) name)
    []
    (formatHeadCommented tabSize (formatType elmVersion tabSize) typ)


formatPattern :: ElmVersion -> Int -> Bool -> AST.Pattern.Pattern -> Box
formatPattern elmVersion tabSize parensRequired apattern =
    case RA.drop apattern of
        AST.Pattern.Anything ->
            line $ keyword "_"

        AST.Pattern.UnitPattern comments ->
            formatUnit tabSize '(' ')' comments

        AST.Pattern.Literal lit ->
            formatLiteral lit

        AST.Pattern.VarPattern var ->
            line $ formatLowercaseIdentifier elmVersion [] var

        AST.Pattern.OpPattern (SymbolIdentifier name) ->
            line $ identifier $ "(" ++ name ++ ")"

        AST.Pattern.ConsPattern first rest ->
            let
                formatRight (preOp, postOp, term, eol) =
                    ( False
                    , preOp
                    , line $ punc "::"
                    , formatCommented
                        tabSize
                        (formatEolCommented tabSize $ formatPattern elmVersion tabSize True)
                        (Commented postOp (term, eol) [])
                    )
            in
                formatBinary
                    tabSize
                    False
                    (formatEolCommented tabSize (formatPattern elmVersion tabSize True) first)
                    (map formatRight rest)
                |> if parensRequired then parens tabSize else id

        AST.Pattern.Data ctor [] ->
            line (formatQualifiedUppercaseIdentifier elmVersion ctor)
                |>
                    case (elmVersion, ctor) of
                        (Elm_0_16, [_]) ->
                            id
                        (Elm_0_16, _) ->
                            if parensRequired then parens tabSize else id
                        _ ->
                            id

        AST.Pattern.Data ctor patterns ->
            ElmStructure.application
                (FAJoinFirst JoinAll)
                (line $ formatQualifiedUppercaseIdentifier elmVersion ctor)
                (map (formatHeadCommented tabSize $ formatPattern elmVersion tabSize True) patterns)
            |> if parensRequired then parens tabSize else id

        AST.Pattern.PatternParens pattern ->
            formatCommented tabSize (formatPattern elmVersion tabSize False) pattern
              |> parens tabSize

        AST.Pattern.Tuple patterns ->
            ElmStructure.group tabSize True "(" "," ")" False $ map (formatCommented tabSize $ formatPattern elmVersion tabSize False) patterns

        AST.Pattern.EmptyListPattern comments ->
            formatUnit tabSize '[' ']' comments

        AST.Pattern.List patterns ->
            ElmStructure.group tabSize True "[" "," "]" False $ map (formatCommented tabSize $ formatPattern elmVersion tabSize False) patterns

        AST.Pattern.Record fields ->
            ElmStructure.group tabSize True "{" "," "}" False $ map (formatCommented tabSize $ line . formatLowercaseIdentifier elmVersion []) fields

        AST.Pattern.Alias pattern name ->
          case
            ( formatTailCommented tabSize (formatPattern elmVersion tabSize True) pattern
            , formatHeadCommented tabSize (line . formatLowercaseIdentifier elmVersion []) name
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

          |> (if parensRequired then parens tabSize else id)


formatRecordPair :: ElmVersion -> Int -> String -> (v -> Box) -> (Commented LowercaseIdentifier, Commented v, Bool) -> Box
formatRecordPair elmVersion tabSize delim formatValue (Commented pre k postK, v, forceMultiline) =
    ElmStructure.equalsPair delim forceMultiline
      (formatCommented tabSize (line . formatLowercaseIdentifier elmVersion []) $ Commented [] k postK)
      (formatCommented tabSize formatValue v)
    |> (\x -> Commented pre x []) |> formatCommented tabSize id


formatPair :: Int -> (a -> Line) -> String -> (b -> Box) -> Pair a b -> Box
formatPair tabSize formatA delim formatB (Pair a b (ForceMultiline forceMultiline)) =
    ElmStructure.equalsPair delim forceMultiline
        (formatTailCommented tabSize (line . formatA) a)
        (formatHeadCommented tabSize formatB b)


negativeCasePatternWorkaround :: Int -> Commented AST.Pattern.Pattern -> Box -> Box
negativeCasePatternWorkaround tabSize (Commented _ (RA.A _ pattern) _) =
    case pattern of
        AST.Pattern.Literal (IntNum i _) | i < 0 -> parens tabSize
        AST.Pattern.Literal (FloatNum f _) | f < 0 -> parens tabSize
        _ -> id


data ExpressionContext
    = SyntaxSeparated
    | InfixSeparated
    | SpaceSeparated
    | AmbiguousEnd


expressionParens :: Int -> ExpressionContext -> ExpressionContext -> Box -> Box
expressionParens tabSize inner outer =
    case (inner, outer) of
        (SpaceSeparated, SpaceSeparated) -> parens tabSize
        (InfixSeparated, SpaceSeparated) -> parens tabSize
        (InfixSeparated, InfixSeparated) -> parens tabSize
        (AmbiguousEnd, SpaceSeparated) -> parens tabSize
        (AmbiguousEnd, InfixSeparated) -> parens tabSize
        (InfixSeparated, AmbiguousEnd) -> parens tabSize
        _ -> id


formatExpression :: ElmVersion -> Int -> ExpressionContext -> AST.Expression.Expr -> Box
formatExpression elmVersion tabSize context aexpr =
    case RA.drop aexpr of
        AST.Expression.Literal lit ->
            formatLiteral lit

        AST.Expression.VarExpr v ->
            line $ formatVar elmVersion v

        AST.Expression.Range left right multiline ->
            case elmVersion of
                Elm_0_16 -> formatRange_0_17 elmVersion tabSize left right multiline
                Elm_0_17 -> formatRange_0_17 elmVersion tabSize left right multiline
                Elm_0_18 -> formatRange_0_17 elmVersion tabSize left right multiline
                Elm_0_18_Upgrade -> formatRange_0_18 elmVersion tabSize context left right

        AST.Expression.ExplicitList exprs trailing multiline ->
            formatSequence tabSize '[' ',' (Just ']')
                (formatExpression elmVersion tabSize SyntaxSeparated)
                multiline
                trailing
                exprs

        AST.Expression.Binops left ops multiline ->
            case elmVersion of
                Elm_0_16 -> formatBinops_0_17 elmVersion tabSize left ops multiline
                Elm_0_17 -> formatBinops_0_17 elmVersion tabSize left ops multiline
                Elm_0_18 -> formatBinops_0_17 elmVersion tabSize left ops multiline
                Elm_0_18_Upgrade -> formatBinops_0_18 elmVersion tabSize left ops multiline
            |> expressionParens tabSize InfixSeparated context

        AST.Expression.Lambda patterns bodyComments expr multiline ->
            case
                ( multiline
                , allSingles $ map (formatCommented tabSize (formatPattern elmVersion tabSize True) . (\(c,p) -> Commented c p [])) patterns
                , bodyComments == []
                , formatExpression elmVersion tabSize SyntaxSeparated expr
                )
            of
                (False, Right patterns', True, SingleLine expr') ->
                    line $ row
                        [ punc "\\"
                        , row $ List.intersperse space $ patterns'
                        , space
                        , punc "->"
                        , space
                        , expr'
                        ]
                (_, Right patterns', _, expr') ->
                    stack1
                        [ line $ row
                            [ punc "\\"
                            , row $ List.intersperse space $ patterns'
                            , space
                            , punc "->"
                            ]
                        , indent $ stack1 $
                            (map (formatComment tabSize) bodyComments)
                            ++ [ expr' ]
                        ]
                (_, Left [], _, _) ->
                    pleaseReport "UNEXPECTED LAMBDA" "no patterns"
                (_, Left patterns', _, expr') ->
                    stack1
                        [ prefix tabSize (punc "\\") $ stack1 patterns'
                        , line $ punc "->"
                        , indent $ stack1 $
                            (map (formatComment tabSize) bodyComments)
                            ++ [ expr' ]
                        ]
            |> expressionParens tabSize AmbiguousEnd context

        AST.Expression.Unary AST.Expression.Negative e ->
            prefix tabSize (punc "-") $ formatExpression elmVersion tabSize SpaceSeparated e -- TODO: This might need something stronger than SpaceSeparated?

        AST.Expression.App left args multiline ->
          ElmStructure.application
            multiline
            (formatExpression elmVersion tabSize InfixSeparated left)
            (map (\(x,y) -> formatCommented' tabSize x (formatExpression elmVersion tabSize SpaceSeparated) y) args)
            |> expressionParens tabSize SpaceSeparated context

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
                        [ opening (line $ keyword "if") $ formatCommented tabSize (formatExpression elmVersion tabSize SyntaxSeparated) cond
                        , indent $ formatCommented_ tabSize True (formatExpression elmVersion tabSize SyntaxSeparated) body
                        ]

                formatElseIf (ifComments, (cond, body)) =
                  let
                    key =
                      case (formatHeadCommented tabSize id (ifComments, line $ keyword "if")) of
                        SingleLine key' ->
                          line $ row [ keyword "else", space, key' ]
                        key' ->
                          stack1
                            [ line $ keyword "else"
                            , key'
                            ]
                  in
                    stack1
                      [ blankLine
                      , opening key $ formatCommented tabSize (formatExpression elmVersion tabSize SyntaxSeparated) cond
                      , indent $ formatCommented_ tabSize True (formatExpression elmVersion tabSize SyntaxSeparated) body
                      ]
            in
                formatIf if'
                    |> andThen (map formatElseIf elseifs)
                    |> andThen
                        [ blankLine
                        , line $ keyword "else"
                        , indent $ formatCommented_ tabSize True (formatExpression elmVersion tabSize SyntaxSeparated) (Commented elsComments els [])
                        ]
                    |> expressionParens tabSize AmbiguousEnd context

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
                      formatDefinition elmVersion tabSize name args comments expr'
                    AST.Expression.LetAnnotation name typ ->
                      formatTypeAnnotation elmVersion tabSize name typ

                    AST.Expression.LetComment comment ->
                        (formatComment tabSize) comment
            in
                (line $ keyword "let")
                    |> andThen
                        (defs
                            |> intersperseMap spacer formatDefinition'
                            |> map indent
                        )
                    |> andThen
                        [ line $ keyword "in"
                        , stack1 $
                            (map (formatComment tabSize) bodyComments)
                            ++ [formatExpression elmVersion tabSize SyntaxSeparated expr]
                        ]
                    |> expressionParens tabSize AmbiguousEnd context -- TODO: not tested

        AST.Expression.Case (subject,multiline) clauses ->
            let
                opening =
                  case
                    ( multiline
                    , formatCommented tabSize (formatExpression elmVersion tabSize SyntaxSeparated) subject
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
                      , (formatPattern elmVersion tabSize False $ (\(Commented _ x _) -> x) pat)
                          |> negativeCasePatternWorkaround tabSize pat
                      , formatCommentedStack tabSize (formatPattern elmVersion tabSize False) pat
                          |> negativeCasePatternWorkaround tabSize pat
                      , formatHeadCommentedStack tabSize (formatExpression elmVersion tabSize SyntaxSeparated) expr
                      )
                    of
                        (_, _, SingleLine pat', body') ->
                            stack1
                                [ line $ row [ pat', space, keyword "->"]
                                , indent body'
                                ]
                        (Commented pre _ [], SingleLine pat', _, body') ->
                            stack1 $
                                (map (formatComment tabSize) pre)
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
                    |> expressionParens tabSize AmbiguousEnd context -- TODO: not tested

        AST.Expression.Tuple exprs multiline ->
            ElmStructure.group tabSize True "(" "," ")" multiline $ map (formatCommented tabSize (formatExpression elmVersion tabSize SyntaxSeparated)) exprs

        AST.Expression.TupleFunction n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"

        AST.Expression.Access expr field ->
            formatExpression elmVersion tabSize SpaceSeparated expr -- TODO: does this need a different context than SpaceSeparated?
                |> addSuffix (row $ [punc ".", formatLowercaseIdentifier elmVersion [] field])

        AST.Expression.AccessFunction (LowercaseIdentifier field) ->
            line $ identifier $ "." ++ (formatVarName' elmVersion field)

        AST.Expression.Record base fields trailing multiline ->
            formatRecordLike
                tabSize
                (line . formatLowercaseIdentifier elmVersion [])
                (formatLowercaseIdentifier elmVersion [])
                "="
                (formatExpression elmVersion tabSize SyntaxSeparated)
                base fields trailing multiline

        AST.Expression.Parens expr ->
            case expr of
                Commented [] expr' [] ->
                    formatExpression elmVersion tabSize context expr'

                _ ->
                    formatCommented tabSize (formatExpression elmVersion tabSize SyntaxSeparated) expr
                        |> parens tabSize


        AST.Expression.Unit comments ->
            formatUnit tabSize '(' ')' comments

        AST.Expression.GLShader src ->
          line $ row
            [ punc "[glsl|"
            , literal $ src
            , punc "|]"
            ]


formatRecordLike ::
    Int
    -> (base -> Box) -> (key -> Line) -> String -> (value -> Box)
    -> Maybe (Commented base) -> Sequence (Pair key value)-> Comments -> ForceMultiline
    -> Box
formatRecordLike tabSize formatBase formatKey fieldSep formatValue base' fields trailing multiline =
    case (base', fields) of
      ( Just base, pairs' ) ->
          ElmStructure.extensionGroup'
              tabSize
              ((\(ForceMultiline b) -> b) multiline)
              (formatCommented tabSize formatBase base)
              (formatSequence
                  tabSize
                  '|' ',' Nothing
                  (formatPair tabSize formatKey fieldSep formatValue)
                  multiline
                  trailing
                  pairs')

      ( Nothing, pairs' ) ->
          formatSequence
              tabSize
              '{' ',' (Just '}')
              (formatPair tabSize formatKey fieldSep formatValue)
              multiline
              trailing
              pairs'


formatSequence :: Int -> Char -> Char -> Maybe Char -> (a -> Box) -> ForceMultiline -> Comments -> Sequence a -> Box
formatSequence tabSize left delim right formatA (ForceMultiline multiline) trailing (first:rest) =
    let
        formatItem delim (pre, item) =
            maybe id (stack' . stack' blankLine) (formatComments tabSize pre) $
            prefix tabSize (row [ punc [delim], space ]) $
            formatHeadCommented tabSize (formatEolCommented tabSize formatA) item
    in
        ElmStructure.forceableSpaceSepOrStack multiline
            (ElmStructure.forceableRowOrStack multiline
                (formatItem left first)
                (map (formatItem delim) rest)
            )
            (maybe [] (flip (:) [] . stack' blankLine) (formatComments tabSize trailing) ++ (Maybe.maybeToList $ fmap (line . punc . flip (:) []) right))
formatSequence tabSize left _ (Just right) _ _ trailing [] =
    formatUnit tabSize left right trailing
formatSequence tabSize left _ Nothing _ _ trailing [] =
    formatUnit tabSize left ' ' trailing


mapIsLast :: (Bool -> a -> b) -> [a] -> [b]
mapIsLast _ [] = []
mapIsLast f (last:[]) = f True last : []
mapIsLast f (next:rest) = f False next : mapIsLast f rest


formatBinops_0_17 ::
    ElmVersion
    -> Int
    -> AST.Expression.Expr
    -> [(Comments, AST.Variable.Ref, Comments, AST.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_17 elmVersion tabSize left ops multiline =
    formatBinops_common (,) elmVersion tabSize left ops multiline


formatBinops_0_18 ::
    ElmVersion
    -> Int
    -> AST.Expression.Expr
    -> [(Comments, AST.Variable.Ref, Comments, AST.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_18 elmVersion tabSize left ops multiline =
    formatBinops_common removeBackticks elmVersion tabSize left ops multiline


formatBinops_common ::
    (AST.Expression.Expr
        -> [(Comments, AST.Variable.Ref, Comments, AST.Expression.Expr)]
        -> ( AST.Expression.Expr
           , [(Comments, AST.Variable.Ref, Comments, AST.Expression.Expr)]
           )
    )
    -> ElmVersion
    -> Int
    -> AST.Expression.Expr
    -> [(Comments, AST.Variable.Ref, Comments, AST.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_common transform elmVersion tabSize left ops multiline =
    let
        (left', ops') = transform left ops

        formatPair isLast ( po, o, pe, e ) =
            let
                isLeftPipe =
                    o == AST.Variable.OpRef (SymbolIdentifier "<|")

                formatContext =
                    if isLeftPipe && isLast
                        then AmbiguousEnd
                        else InfixSeparated
            in
            ( isLeftPipe
            , po
            , (line . formatInfixVar elmVersion) o
            , formatCommented' tabSize pe (formatExpression elmVersion tabSize formatContext) e
            )
    in
        formatBinary
            tabSize
            multiline
            (formatExpression elmVersion tabSize InfixSeparated left')
            (mapIsLast formatPair ops')


removeBackticks ::
    AST.Expression.Expr
    -> [(Comments, AST.Variable.Ref, Comments, AST.Expression.Expr)]
    -> (AST.Expression.Expr, [(Comments, AST.Variable.Ref, Comments, AST.Expression.Expr)])
removeBackticks left ops =
    case ops of
        [] -> (left, ops)
        (pre, AST.Variable.VarRef v' v, post, e):rest
          | v == LowercaseIdentifier "andThen" || v == LowercaseIdentifier "onError"
          ->
            -- Convert `andThen` to |> andThen
            let
                e' = noRegion $ AST.Expression.App
                    (noRegion $ AST.Expression.VarExpr $ AST.Variable.VarRef v' v)
                    [ (post, e)
                    ]
                    (FAJoinFirst JoinAll)

                (e'', rest') = removeBackticks e' rest
            in
                (left, (pre, AST.Variable.OpRef $ SymbolIdentifier "|>", [], e''):rest')

        (pre, AST.Variable.VarRef v' v, post, e):rest ->
            -- Convert other backtick operators to normal function application
            removeBackticks
                (noRegion $ AST.Expression.App
                    (noRegion $ AST.Expression.VarExpr $ AST.Variable.VarRef v' v)
                    [ (pre, left)
                    , (post, e)
                    ]
                    (FAJoinFirst JoinAll)
                )
                rest

        (pre, op, post, e):rest ->
            -- Preserve symbolic infix operators
            let
                (e', rest') = removeBackticks e rest
            in
                (left, (pre, op, post, e'):rest')


formatRange_0_17 :: ElmVersion -> Int -> Commented AST.Expression.Expr -> Commented AST.Expression.Expr -> Bool -> Box
formatRange_0_17 elmVersion tabSize left right multiline =
    case
        ( multiline
        , formatCommented tabSize (formatExpression elmVersion tabSize SyntaxSeparated) left
        , formatCommented tabSize (formatExpression elmVersion tabSize SyntaxSeparated) right
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

nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> RA.Located a
noRegion =
    RA.at nowhere nowhere

formatRange_0_18 :: ElmVersion -> Int -> ExpressionContext -> Commented AST.Expression.Expr -> Commented AST.Expression.Expr -> Box
formatRange_0_18 elmVersion tabSize context left right =
    case (left, right) of
        (Commented preLeft left' [], Commented preRight right' []) ->
            AST.Expression.App
                (noRegion $ AST.Expression.VarExpr $ AST.Variable.VarRef [UppercaseIdentifier "List"] $ LowercaseIdentifier "range")
                [ (preLeft, left')
                , (preRight, right')
                ]
                (FAJoinFirst JoinAll)
                |> noRegion
                |> formatExpression elmVersion tabSize context

        _ ->
            AST.Expression.App
                (noRegion $ AST.Expression.VarExpr $ AST.Variable.VarRef [UppercaseIdentifier "List"] $ LowercaseIdentifier "range")
                [ ([], noRegion $ AST.Expression.Parens left)
                , ([], noRegion $ AST.Expression.Parens right)
                ]
                (FAJoinFirst JoinAll)
                |> noRegion
                |> formatExpression elmVersion tabSize context


formatUnit :: Int -> Char -> Char -> Comments -> Box
formatUnit tabSize left right comments =
  case (left, comments) of
    (_, []) ->
      line $ punc (left : right : [])

    ('{', (LineComment _):_) ->
      surround tabSize left right $ prefix tabSize space $ stack1 $ map (formatComment tabSize) comments

    _ ->
      surround tabSize left right $
        case allSingles $ map (formatComment tabSize) comments of
          Right comments' ->
            line $ row $ List.intersperse space comments'

          Left comments' ->
            stack1 comments'


formatComments :: Int -> Comments -> Maybe Box
formatComments tabSize comments =
    case fmap (formatComment tabSize) comments of
        [] ->
            Nothing

        (first:rest) ->
            Just $ ElmStructure.spaceSepOrStack first rest


formatCommented_ :: Int -> Bool -> (a -> Box) -> Commented a -> Box
formatCommented_ tabSize forceMultiline format (Commented pre inner post) =
    ElmStructure.forceableSpaceSepOrStack1 forceMultiline $
        concat
            [ Maybe.maybeToList $ (formatComments tabSize) pre
            , [format inner]
            , Maybe.maybeToList $ (formatComments tabSize) post
            ]


formatCommented :: Int -> (a -> Box) -> Commented a -> Box
formatCommented tabSize =
  formatCommented_ tabSize False


-- TODO: rename to formatPreCommented
formatHeadCommented :: Int -> (a -> Box) -> (Comments, a) -> Box
formatHeadCommented tabSize format (pre, inner) =
    formatCommented' tabSize pre format inner


formatCommented' :: Int -> Comments -> (a -> Box) -> a -> Box
formatCommented' tabSize pre format inner =
    formatCommented tabSize format (Commented pre inner [])


formatTailCommented :: Int -> (a -> Box) -> (a, Comments) -> Box
formatTailCommented tabSize format (inner, post) =
  formatCommented tabSize format (Commented [] inner post)


formatEolCommented :: Int -> (a -> Box) -> WithEol a -> Box
formatEolCommented tabSize format (inner, post) =
  case (post, format inner) of
    (Nothing, box) -> box
    (Just eol, SingleLine result) ->
      mustBreak $ row [ result, space, punc "--", literal eol ]
    (Just eol, box) ->
      stack1 [ box, formatComment tabSize $ LineComment eol ]


formatCommentedStack :: Int -> (a -> Box) -> Commented a -> Box
formatCommentedStack tabSize format (Commented pre inner post) =
  stack1 $
    (map (formatComment tabSize) pre)
      ++ [ format inner ]
      ++ (map (formatComment tabSize) post)


formatHeadCommentedStack :: Int -> (a -> Box) -> (Comments, a) -> Box
formatHeadCommentedStack tabSize format (pre, inner) =
  formatCommentedStack tabSize format (Commented pre inner [])


formatKeywordCommented :: Int -> String -> (a -> Box) -> KeywordCommented a -> Box
formatKeywordCommented tabSize word format (KeywordCommented pre post value) =
  ElmStructure.spaceSepOrIndented
    (formatCommented tabSize (line . keyword) (Commented pre word post))
    [ format value ]


formatOpenCommentedList :: Int -> (a -> Box) -> OpenCommentedList a -> [Box]
formatOpenCommentedList tabSize format (OpenCommentedList rest (preLst, lst)) =
    (fmap (formatCommented tabSize $ formatEolCommented tabSize format) rest)
        ++ [formatCommented tabSize (formatEolCommented tabSize format) $ Commented preLst lst []]


formatComment :: Int -> Comment -> Box
formatComment tabSize comment =
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
                            tabSize
                            (row [ punc "{-", space ])
                            (stack1 $ map (line . literal) ls)
                        , line $ punc "-}"
                        ]

        LineComment c ->
            mustBreak $ row [ punc "--", literal c ]

        CommentTrickOpener ->
            mustBreak $ punc "{--}"

        CommentTrickCloser ->
            mustBreak $ punc "--}"

        CommentTrickBlock c ->
            mustBreak $ row [ punc "{--", literal c, punc "-}" ]


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
            step okay quotes remaining =
                case remaining of
                    [] ->
                        reverse $ (concat $ replicate quotes "\"\\") ++ okay

                    next : rest ->
                        if next == '"' then
                            step okay (quotes + 1) rest
                        else if quotes >= 3 then
                            step (next : (concat $ replicate quotes "\"\\") ++ okay) 0 rest
                        else if quotes > 0 then
                            step (next : (replicate quotes '"') ++ okay) 0 rest
                        else
                            step (next : okay) 0 rest
        in
            step "" 0



data TypeParensRequired
    = ForLambda
    | ForCtor
    | NotRequired
    deriving (Eq)


formatType :: ElmVersion -> Int -> Type -> Box
formatType elmVersion tabSize =
    formatType' elmVersion tabSize NotRequired


commaSpace :: Line
commaSpace =
    row
        [ punc ","
        , space
        ]


formatTypeConstructor :: ElmVersion -> TypeConstructor -> Box
formatTypeConstructor elmVersion ctor =
    case ctor of
        NamedConstructor name ->
            line $ formatQualifiedUppercaseIdentifier elmVersion name

        TupleConstructor n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"


formatType' :: ElmVersion -> Int -> TypeParensRequired -> Type -> Box
formatType' elmVersion tabSize requireParens atype =
    case RA.drop atype of
        UnitType comments ->
          formatUnit tabSize '(' ')' comments

        FunctionType first rest (ForceMultiline forceMultiline) ->
            let
                formatRight (preOp, postOp, term, eol) =
                    ElmStructure.forceableSpaceSepOrStack1
                        False
                        $ concat
                            [ Maybe.maybeToList $ formatComments tabSize preOp
                            , [ ElmStructure.prefixOrIndented
                                  (line $ punc "->")
                                  (formatCommented
                                      tabSize
                                      (formatEolCommented tabSize $ formatType' elmVersion tabSize ForLambda)
                                      (Commented postOp (term, eol) [])
                                  )
                              ]
                            ]
            in
                ElmStructure.forceableSpaceSepOrStack
                    forceMultiline
                    (formatEolCommented tabSize (formatType' elmVersion tabSize ForLambda) first)
                    (map formatRight rest)
                |> if requireParens /= NotRequired then parens tabSize else id

        TypeVariable var ->
            line $ identifier $ formatVarName elmVersion var

        TypeConstruction ctor args ->
            ElmStructure.application
                (FAJoinFirst JoinAll)
                (formatTypeConstructor elmVersion ctor)
                (map (formatHeadCommented tabSize $ formatType' elmVersion tabSize ForCtor) args)
                |> (if args /= [] && requireParens == ForCtor then parens tabSize else id)

        TypeParens type' ->
          parens tabSize $ formatCommented tabSize (formatType elmVersion tabSize) type'

        TupleType types ->
          ElmStructure.group tabSize True "(" "," ")" False (map (formatCommented tabSize (formatEolCommented tabSize $ formatType elmVersion tabSize)) types)

        RecordType base fields trailing multiline ->
            formatRecordLike
                tabSize
                (line . formatLowercaseIdentifier elmVersion [])
                (formatLowercaseIdentifier elmVersion [])
                ":"
                (formatType elmVersion tabSize)
                base fields trailing multiline


formatVar :: ElmVersion -> AST.Variable.Ref -> Line
formatVar elmVersion var =
    case var of
        AST.Variable.VarRef namespace name ->
            formatLowercaseIdentifier elmVersion namespace name
        AST.Variable.TagRef namespace name ->
            case namespace of
                [] -> identifier $ formatVarName'' elmVersion name
                _ ->
                    row
                        [ formatQualifiedUppercaseIdentifier elmVersion namespace
                        , punc "."
                        , identifier $ formatVarName'' elmVersion name
                        ]
        AST.Variable.OpRef (SymbolIdentifier name) ->
            identifier $ "(" ++ name ++ ")"


formatInfixVar :: ElmVersion -> AST.Variable.Ref -> Line
formatInfixVar elmVersion var =
    case var of
        AST.Variable.VarRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        AST.Variable.TagRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        AST.Variable.OpRef (SymbolIdentifier name) ->
            identifier name


formatLowercaseIdentifier :: ElmVersion -> [UppercaseIdentifier] -> LowercaseIdentifier -> Line
formatLowercaseIdentifier elmVersion namespace (LowercaseIdentifier name) =
    case (elmVersion, namespace, name) of
        (Elm_0_18_Upgrade, [], "fst") -> identifier "Tuple.first"
        (Elm_0_18_Upgrade, [UppercaseIdentifier "Basics"], "fst") -> identifier "Tuple.first"
        (Elm_0_18_Upgrade, [], "snd") -> identifier "Tuple.second"
        (Elm_0_18_Upgrade, [UppercaseIdentifier "Basics"], "snd") -> identifier "Tuple.second"
        (_, [], _) -> identifier $ formatVarName' elmVersion name
        _ ->
            row
                [ formatQualifiedUppercaseIdentifier elmVersion namespace
                , punc "."
                , identifier $ formatVarName' elmVersion name
                ]


formatUppercaseIdentifier :: ElmVersion -> UppercaseIdentifier -> Line
formatUppercaseIdentifier elmVersion (UppercaseIdentifier name) =
    identifier $ formatVarName' elmVersion name


formatQualifiedUppercaseIdentifier :: ElmVersion -> [UppercaseIdentifier] -> Line
formatQualifiedUppercaseIdentifier elmVersion names =
  identifier $ List.intercalate "." $
      map (\(UppercaseIdentifier name) -> formatVarName' elmVersion name) names


formatVarName :: ElmVersion -> LowercaseIdentifier -> String
formatVarName elmVersion (LowercaseIdentifier name) =
    formatVarName' elmVersion name


formatVarName' :: ElmVersion -> String -> String
formatVarName' elmVersion name =
    case elmVersion of
        Elm_0_18_Upgrade -> map (\x -> if x == '\'' then '_' else x) name
        _ -> name


formatVarName'' :: ElmVersion -> UppercaseIdentifier -> String
formatVarName'' elmVersion (UppercaseIdentifier name) =
    formatVarName' elmVersion name
