{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module ElmFormat.Upgrade_0_19 (transform, parseUpgradeDefinition, transformModule) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Declaration (Declaration(..), TopLevelStructure(..))
import AST.Expression
import AST.MapExpr
import AST.Module (Module(Module))
import AST.Pattern
import AST.Variable
import ElmVersion
import Reporting.Annotation (Located(A))

import qualified Data.List as List
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified ElmFormat.Parse
import qualified ElmFormat.Version
import qualified Reporting.Annotation as RA
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result
import qualified ReversedList


upgradeDefinition :: Text.Text
upgradeDefinition = Text.pack $ unlines
    [ "upgrade_flip f b a ="
    , "    f a b"
    , ""
    , "upgrade_curry f a b ="
    , "    f (a, b)"
    , ""
    , "upgrade_uncurry f (a, b) ="
    , "    f a b"
    , ""
    , "upgrade_rem dividend divisor ="
    , "    remainderBy divisor dividend"
    ]

data UpgradeDefinition =
    UpgradeDefinition
        { replacements :: Dict.Map String Expr'
        }
    deriving Show

parseUpgradeDefinition :: Text.Text -> Either () UpgradeDefinition
parseUpgradeDefinition definitionText =
    case ElmFormat.Parse.parse Elm_0_19 definitionText of
        Result.Result _ (Result.Ok (Module _ _ _ _ body)) ->
            let
                toUpgradeDef def =
                    case def of
                        Entry (A _ (Definition (A _ (VarPattern (LowercaseIdentifier name))) [] _ (A _ upgradeBody))) ->
                            case List.stripPrefix "upgrade_" name of
                                Just functionName -> Just (functionName, upgradeBody)
                                Nothing -> Nothing

                        Entry (A _ (Definition (A _ (VarPattern (LowercaseIdentifier name))) args comments upgradeBody)) ->
                            case List.stripPrefix "upgrade_" name of
                                Just functionName ->
                                    Just
                                        ( functionName
                                        , Lambda args comments upgradeBody False
                                        )

                                Nothing -> Nothing

                        _ ->
                            Nothing
            in
            Right $ UpgradeDefinition
                { replacements = Dict.fromList $ Maybe.mapMaybe toUpgradeDef body
                }

        Result.Result _ (Result.Err _) ->
            Left ()

transform ::
    Dict.Map LowercaseIdentifier [UppercaseIdentifier]
    -> Dict.Map [UppercaseIdentifier] UppercaseIdentifier
    -> Expr -> Expr
transform =
    case parseUpgradeDefinition upgradeDefinition of
        Right replacements ->
            transform' replacements

        Left () ->
            error "Couldn't parse upgrade definition"


transformModule :: UpgradeDefinition -> Module -> Module
transformModule definition mod =
    let
        exposed = Dict.empty
        importAliases = Dict.empty

        transformTopLevelStructure structure =
            case structure of
                Entry (A region (Definition name args comments expr)) ->
                    Entry (A region (Definition name args comments $ visitExprs (transform' definition exposed importAliases) expr))

                _ -> structure
    in
    case mod of
        Module a b c d body ->
            Module a b c d (fmap transformTopLevelStructure body)


visitLetDeclaration :: (Expr -> Expr) -> LetDeclaration -> LetDeclaration
visitLetDeclaration f d =
    case d of
        LetDefinition name args pre body -> LetDefinition name args pre (visitExprs f body)
        _ -> d


visitExprs :: (Expr -> Expr) -> Expr -> Expr
visitExprs f original@(A region _) =
    let expr = f original
    in
    case RA.drop expr of
        Unit _ -> expr
        AST.Expression.Literal _ -> expr
        VarExpr _ -> expr

        App f' args multiline ->
            A region $ App (visitExprs f f') (fmap (fmap $ visitExprs f) args) multiline
        Unary op e ->
            A region $ Unary op (visitExprs f e)
        Binops left restOps multiline ->
            A region $ Binops (visitExprs f left) (fmap (\(a,b,c,d) -> (a, b, c, visitExprs f d)) restOps) multiline
        Parens e ->
            A region $ Parens (fmap (visitExprs f) e)
        ExplicitList terms' post multiline ->
            A region $ ExplicitList (fmap (fmap $ fmap $ fmap $ visitExprs f) terms') post multiline
        Range e1 e2 multiline ->
            A region $ Range (fmap (visitExprs f) e1) (fmap (visitExprs f) e2) multiline
        AST.Expression.Tuple es multiline ->
            A region $ AST.Expression.Tuple (fmap (fmap $ visitExprs f) es) multiline
        TupleFunction _ -> expr
        AST.Expression.Record b fs post multiline ->
            A region $ AST.Expression.Record b (fmap (fmap $ fmap $ fmap $ fmap $ visitExprs f) fs) post multiline
        Access e field' ->
            A region $ Access (visitExprs f e) field'
        AccessFunction _ -> expr
        Lambda params pre body multi ->
            A region $ Lambda params pre (visitExprs f body) multi
        If (c, b) elseIfs els ->
            A region $ If (fmap (visitExprs f) c, fmap (visitExprs f) b) (fmap (fmap (\(c', b') -> (fmap (visitExprs f) c', fmap (visitExprs f) b'))) elseIfs) (fmap (visitExprs f) els)
        Let decls pre body ->
            A region $ Let (fmap (visitLetDeclaration f) decls) pre body
        Case (e, b) branches ->
            A region $ Case (fmap (visitExprs f) e, b) (fmap (fmap $ fmap $ visitExprs f) branches)
        GLShader _ -> expr


transform' ::
    UpgradeDefinition
    -> Dict.Map LowercaseIdentifier [UppercaseIdentifier]
    -> Dict.Map [UppercaseIdentifier] UppercaseIdentifier
    -> Expr -> Expr
transform' (UpgradeDefinition basicsReplacements) exposed importAliases expr =
    let
        replace var =
            case var of
                VarRef [] (LowercaseIdentifier name) ->
                    Dict.lookup name basicsReplacements

                VarRef [(UppercaseIdentifier "Basics")] (LowercaseIdentifier name) ->
                    Dict.lookup name basicsReplacements

                OpRef (SymbolIdentifier "!") ->
                    Just $
                    Lambda
                      [makeArg "model", makeArg "cmds"] []
                      (noRegion $ Binops
                          (makeVarRef "model")
                          [([], var, [], makeVarRef "cmds")]
                          False
                      )
                      False

                OpRef (SymbolIdentifier "%") ->
                    Just $
                    Lambda
                      [makeArg "dividend", makeArg "modulus"] []
                      (noRegion $ App
                          (makeVarRef "modBy")
                          [ ([], makeVarRef "modulus")
                          , ([], makeVarRef "dividend")
                          ]
                          (FAJoinFirst JoinAll)
                      )
                      False

                _ -> Nothing

        makeTuple n =
            let
                vars =
                  if n <= 26
                    then fmap (\c -> [c]) (take n ['a'..'z'])
                    else error (pleaseReport'' "UNEXPECTED TUPLE" "more than 26 elements")
            in
                Lambda
                    (fmap makeArg vars)
                    []
                    (noRegion $ AST.Expression.Tuple (fmap (\v -> Commented [] (makeVarRef v) []) vars) False)
                    False
    in
    case RA.drop expr of
        VarExpr var ->
            Maybe.fromMaybe expr $ fmap noRegion $ replace var

        App (A _ (VarExpr var)) args multiline ->
            Maybe.fromMaybe expr $ fmap (\new -> applyLambda (noRegion new) args multiline) $ replace var

        TupleFunction n ->
            noRegion $ makeTuple n

        App (A _ (TupleFunction n)) args multiline ->
            applyLambda (noRegion $ makeTuple n) args multiline

        ExplicitList terms' trailing multiline ->
            let
                ha = (fmap UppercaseIdentifier ["Html", "Attributes"])
                styleExposed = Dict.lookup (LowercaseIdentifier "style") exposed == Just ha
                haAlias = Dict.lookup ha importAliases
            in
            noRegion $ ExplicitList (concat $ fmap (expandHtmlStyle styleExposed haAlias) $ terms') trailing multiline

        _ ->
            expr


expandHtmlStyle :: Bool -> Maybe UppercaseIdentifier -> (Comments, PreCommented (WithEol Expr)) -> [(Comments, PreCommented (WithEol Expr))]
expandHtmlStyle styleExposed importAlias (preComma, (pre, WithEol term eol)) =
    let
        lambda fRef =
            Lambda
                [([], noRegion $ AST.Pattern.Tuple [makeArg' "a", makeArg' "b"]) ] []
                (noRegion $ App
                    (noRegion $ VarExpr $ fRef)
                    [ ([], makeVarRef "a")
                    , ([], makeVarRef "b")
                    ]
                    (FAJoinFirst JoinAll)
                )
                False

        isHtmlAttributesStyle var =
            case var of
                VarRef [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"] (LowercaseIdentifier "style") -> True
                VarRef [alias] (LowercaseIdentifier "style") | Just alias == importAlias -> True
                VarRef [] (LowercaseIdentifier "style") -> styleExposed
                _ -> False
    in
    case RA.drop term of
        App (A _ (VarExpr var)) [(preStyle, A _ (ExplicitList styles trailing _))] _ | isHtmlAttributesStyle var ->
            fmap (\(preComma', (pre', WithEol style eol')) -> (preComma ++ preComma', (pre ++ preStyle ++ pre' ++ trailing ++ (Maybe.maybeToList $ fmap LineComment eol), WithEol (applyLambda (noRegion $ lambda var) [([], style)] (FAJoinFirst JoinAll)) eol'))) styles

        _ ->
            [(preComma, (pre, WithEol term eol))]

--
-- Generic helpers
--


pleaseReport'' :: String -> String -> String
pleaseReport'' what details =
    "<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"



nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> RA.Located a
noRegion =
    RA.at nowhere nowhere


makeArg :: String -> (Comments, Pattern)
makeArg varName =
    ([], noRegion $ VarPattern $ LowercaseIdentifier varName)


makeArg' :: String -> Commented Pattern
makeArg' varName =
    Commented [] (noRegion $ VarPattern $ LowercaseIdentifier varName) []


makeVarRef :: String -> Expr
makeVarRef varName =
    noRegion $ VarExpr $ VarRef [] $ LowercaseIdentifier varName


inlineVar :: LowercaseIdentifier -> Bool -> Expr' -> Expr' -> Expr'
inlineVar name insertMultiline value expr =
    Maybe.fromMaybe expr $ inlineVar' name insertMultiline value expr


inlineVar' :: LowercaseIdentifier -> Bool -> Expr' -> Expr' -> Maybe Expr'
inlineVar' name insertMultiline value expr =
    case expr of
        VarExpr (VarRef [] n) | n == name -> Just value

        AST.Expression.Tuple terms' multiline ->
            let
                step (acc, expand) t@(Commented pre (A _ term) post) =
                    case inlineVar' name insertMultiline value term of
                        Nothing -> (ReversedList.push t acc, expand)
                        Just term' -> (ReversedList.push (Commented pre (noRegion term') post) acc, insertMultiline || expand)

                (terms'', multiline'') = foldl step (ReversedList.empty, multiline) terms'
            in
            Just $ AST.Expression.Tuple (ReversedList.toList terms'') multiline''

        -- TODO: handle expanding multiline in contexts other than tuples

        AST.Expression.Case (Commented pre (A _ term) post, _) branches@((Commented _ (A _ p1) _, (_, A _ b1)):_) ->
            let
                literalMatch pat exp =
                    case (pat, exp) of
                        (Anything, _) -> True -- TODO: not tested
                        (Data name [], VarExpr (TagRef ns tag)) | name == (ns ++ [tag]) -> True
                        _ -> False
            in
            case inlineVar' name insertMultiline value term of
                Nothing -> Nothing
                Just term' ->
                    if literalMatch p1 term'
                        then Just b1
                        else Just $ AST.Expression.Case (Commented pre (noRegion term') post, False) branches

        _ -> Just $ mapExpr (inlineVar name insertMultiline value) expr


applyLambda :: Expr -> [PreCommented Expr] -> FunctionApplicationMultiline -> Expr
applyLambda lambda args appMultiline =
    let
        getMapping :: PreCommented Pattern -> PreCommented Expr -> Maybe [(LowercaseIdentifier, Expr')]
        getMapping pat arg =
            case (pat, arg) of
                ( (preVar, A _ (VarPattern name))
                 , (preArg, arg')
                 ) ->
                    Just [(name, Parens $ Commented (preVar ++ preArg) arg' [])]

                ( (preVar, A _ (AST.Pattern.Tuple [Commented preA (A _ (VarPattern nameA)) postA, Commented preB (A _ (VarPattern nameB)) postB]))
                 , (preArg, A _ (AST.Expression.Tuple [Commented preAe eA postAe, Commented preBe eB postBe] _))
                 ) ->
                    Just
                        [ (nameA, Parens $ Commented (preVar ++ preArg) (noRegion $ Parens $ Commented (preA ++ preAe) eA (postAe ++ postA)) [])
                        , (nameB, Parens $ Commented (preB ++ preBe) eB (postBe ++ postB))
                        ]

                ( (preVar, A _ (AST.Pattern.Record varFields))
                  , (preArg, A _ (AST.Expression.Record _ argFields _ _))
                  ) ->
                    let
                        args :: Dict.Map LowercaseIdentifier Expr'
                        args =
                            argFields
                                |> fmap snd
                                |> fmap snd
                                |> fmap (\(WithEol a _) -> a)
                                |> fmap (\(Pair (k, _) v _) -> (k, RA.drop $ snd v))
                                |> Dict.fromList

                        fieldMapping :: Commented LowercaseIdentifier -> Maybe (LowercaseIdentifier, Expr')
                        fieldMapping (Commented _ var _) =
                            (,) var <$> Dict.lookup var args
                    in
                    sequence $ fmap fieldMapping varFields

                _ ->
                    Nothing
    in
    case (RA.drop lambda, args) of
        (Lambda (pat:restVar) preBody body multiline, arg:restArgs) ->
            case getMapping pat arg of
                Nothing ->
                    -- failed to destructure the next argument, so stop
                    noRegion $ App lambda args appMultiline

                Just mappings ->
                    let
                        newBody = foldl (\e (name, value) -> mapExpr (inlineVar name (appMultiline == FASplitFirst) value) e) body mappings
                        newMultiline =
                            case appMultiline of
                                FASplitFirst -> FASplitFirst
                                FAJoinFirst SplitAll -> FASplitFirst
                                FAJoinFirst JoinAll -> FAJoinFirst JoinAll
                    in
                    case restVar of
                        [] ->
                            -- we applied the argument and none are left, so remove the lambda
                            noRegion $ App (noRegion $ Parens $ Commented preBody newBody []) restArgs newMultiline
                        _:_ ->
                            -- we applied this argument; try to apply the next argument
                            applyLambda (noRegion $ Lambda restVar preBody newBody multiline) restArgs newMultiline

        (_, []) -> lambda

        _ -> noRegion $ App lambda args appMultiline
