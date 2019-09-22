{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module ElmFormat.Upgrade_0_19 (transform, parseUpgradeDefinition, transformModule) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Declaration (Declaration(..), TopLevelStructure(..))
import AST.Expression
import AST.MapExpr
import AST.MapNamespace
import AST.Module (Module(Module), ImportMethod(ImportMethod))
import AST.Pattern
import AST.Variable
import Control.Monad (zipWithM)
import Data.Fix
import ElmFormat.ImportInfo (ImportInfo)
import ElmVersion
import Reporting.Annotation (Located(A))

import qualified AST.Module
import qualified Data.Bimap as Bimap
import qualified Data.List as List
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.Parse
import qualified ElmFormat.Version
import qualified Reporting.Annotation as RA
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result
import qualified ReversedList


upgradeDefinition :: Text.Text
upgradeDefinition = Text.pack $ unlines
    [ "upgrade_Basics_flip f b a ="
    , "    f a b"
    , ""
    , "upgrade_Basics_curry f a b ="
    , "    f (a, b)"
    , ""
    , "upgrade_Basics_uncurry f (a, b) ="
    , "    f a b"
    , ""
    , "upgrade_Basics_rem dividend divisor ="
    , "    remainderBy divisor dividend"
    ]


data UpgradeDefinition =
    UpgradeDefinition
        { _replacements :: Dict.Map ([UppercaseIdentifier], LowercaseIdentifier) Expr'
        , _imports :: Dict.Map [UppercaseIdentifier] (Comments, ImportMethod)
        , _preferredAliases :: [(UppercaseIdentifier, [UppercaseIdentifier])]
        }
    deriving Show


parseUpgradeDefinition :: Text.Text -> Either () UpgradeDefinition
parseUpgradeDefinition definitionText =
    case ElmFormat.Parse.parse Elm_0_19 definitionText of
        Result.Result _ (Result.Ok modu@(Module _ _ _ (_, imports) body)) ->
            let
                importInfo = ImportInfo.fromModule modu

                makeName :: String -> Maybe ([UppercaseIdentifier], LowercaseIdentifier)
                makeName name =
                    (\rev -> (UppercaseIdentifier <$> reverse (tail rev), LowercaseIdentifier $ head rev))
                        <$> reverse <$> splitOn '_' <$> List.stripPrefix "upgrade_" name

                toUpgradeDef def =
                    case def of
                        Entry (A _ (Definition (A _ (VarPattern (LowercaseIdentifier name))) [] _ (Fix (LocatedExpression (A _ upgradeBody))))) ->
                            case makeName name of
                                Just functionName -> Just (functionName, upgradeBody)
                                Nothing -> Nothing

                        Entry (A _ (Definition (A _ (VarPattern (LowercaseIdentifier name))) args comments upgradeBody)) ->
                            case makeName name of
                                Just functionName ->
                                    Just
                                        ( functionName
                                        , Lambda args comments upgradeBody False
                                        )

                                Nothing -> Nothing

                        _ ->
                            Nothing

                applyAliases ns =
                    Maybe.fromMaybe ns $
                    case ns of
                        [single] -> Bimap.lookupR single (ImportInfo._aliases importInfo)
                        _ -> Nothing

                removeAlias :: ImportMethod -> ImportMethod
                removeAlias i =
                    i { AST.Module.alias = Nothing }

                getAlias (ns, (_, ImportMethod alias _)) =
                    case alias of
                        Nothing -> Nothing
                        Just (_, (_, a)) -> Just (a, ns)
            in
            Right $ UpgradeDefinition
                { _replacements = fmap (mapNamespace applyAliases) $ Dict.fromList $ Maybe.mapMaybe toUpgradeDef body
                , _imports = fmap (fmap removeAlias) imports
                , _preferredAliases = Maybe.mapMaybe getAlias $ Dict.toList imports
                }

        Result.Result _ (Result.Err _) ->
            Left ()


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s =
    case dropWhile ((==) c) s of
        [] -> []
        s' ->
            w : splitOn c s''
            where
                (w, s'') =
                    break ((==) c) s'


transform ::
    ImportInfo
    -> Expr -> Expr
transform =
    case parseUpgradeDefinition upgradeDefinition of
        Right replacements ->
            transform' replacements

        Left () ->
            error "Couldn't parse upgrade definition"


transformModule :: UpgradeDefinition -> Module -> Module
transformModule upgradeDefinition modu@(Module a b c (preImports, originalImports) originalBody) =
    let
        importInfo =
            -- Note: this is the info used for matching references in the
            -- source file being transformed, and should NOT include
            -- the imports merged in from the upgrade definition
            ImportInfo.fromModule modu

        transformTopLevelStructure structure =
            case structure of
                Entry (A region (Definition name args comments expr)) ->
                    Entry (A region (Definition name args comments $ visitExprs (transform' upgradeDefinition importInfo) expr))

                _ -> structure

        upgradedBody = fmap transformTopLevelStructure originalBody

        expressionFromTopLevelStructure structure =
            case structure of
                Entry (A _ (Definition _ _ _ expr)) -> Just expr
                _ -> Nothing

        collectExprs = Maybe.mapMaybe expressionFromTopLevelStructure upgradedBody

        usagesAfterUpgrade =
            Dict.unionsWith (Dict.unionWith (+)) $
            fmap (cata countUsages . stripRegion) collectExprs

        namespacesWithReplacements =
              Set.fromList $ fmap fst $ Dict.keys $ _replacements upgradeDefinition

        remainingUsages ns =
            Dict.foldr (+) 0 $ Maybe.fromMaybe Dict.empty $ Dict.lookup ns usagesAfterUpgrade

        cleanImports ns (_, importMethod) =
            let
                nameToCheck =
                    case importMethod of
                        ImportMethod (Just (_, (_, alias))) _ -> [alias]
                        _ -> ns
                wasReplaced = Set.member ns namespacesWithReplacements
            in
            if wasReplaced && remainingUsages nameToCheck <= 0
                then False
                else True

        additionalAliases =
            _preferredAliases upgradeDefinition
                |> filter (\(alias, _) -> remainingUsages [alias] <= 0)

        newImports =
            Dict.union
                (Dict.filterWithKey cleanImports originalImports)
                (_imports upgradeDefinition)

        applyAlias (imports, body) (alias, ns) =
            if remainingUsages [alias] <= 0
                then
                    ( Dict.update (\(pre, im) -> Just $ (pre, im { AST.Module.alias = Just ([], ([], alias)) }) ) ns imports
                    , mapNamespace (\n -> if n == ns then [alias] else n) body
                    )
                else (imports, body)

        (finalImports, finalBody) =
            List.foldl' applyAlias (newImports, upgradedBody) additionalAliases
    in
    Module a b c (preImports, finalImports) finalBody


visitLetDeclaration :: (Expr -> Expr) -> LetDeclaration Expr -> LetDeclaration Expr
visitLetDeclaration f d =
    case d of
        LetDefinition name args pre body -> LetDefinition name args pre (visitExprs f body)
        _ -> d


visitExprs :: (Expr -> Expr) -> Expr -> Expr
visitExprs f original@(Fix (LocatedExpression (A region _))) =
    let expr = f original
    in
    case RA.drop $ (\(Fix (LocatedExpression e)) -> e) expr of
        Unit _ -> expr
        AST.Expression.Literal _ -> expr
        VarExpr _ -> expr

        App f' args multiline ->
            Fix $ LocatedExpression $ A region $ App (visitExprs f f') (fmap (fmap $ visitExprs f) args) multiline
        Unary op e ->
            Fix $ LocatedExpression $ A region $ Unary op (visitExprs f e)
        Binops left restOps multiline ->
            Fix $ LocatedExpression $ A region $ Binops (visitExprs f left) (fmap (\(BinopsClause a b c d) -> BinopsClause a b c (visitExprs f d)) restOps) multiline
        Parens e ->
            Fix $ LocatedExpression $ A region $ Parens (fmap (visitExprs f) e)
        ExplicitList terms' post multiline ->
            Fix $ LocatedExpression $ A region $ ExplicitList (fmap (fmap $ fmap $ fmap $ visitExprs f) terms') post multiline
        Range e1 e2 multiline ->
            Fix $ LocatedExpression $ A region $ Range (fmap (visitExprs f) e1) (fmap (visitExprs f) e2) multiline
        AST.Expression.Tuple es multiline ->
            Fix $ LocatedExpression $ A region $ AST.Expression.Tuple (fmap (fmap $ visitExprs f) es) multiline
        TupleFunction _ -> expr
        AST.Expression.Record b fs post multiline ->
            Fix $ LocatedExpression $ A region $ AST.Expression.Record b (fmap (fmap $ fmap $ fmap $ fmap $ visitExprs f) fs) post multiline
        Access e field' ->
            Fix $ LocatedExpression $ A region $ Access (visitExprs f e) field'
        AccessFunction _ -> expr
        Lambda params pre body multi ->
            Fix $ LocatedExpression $ A region $ Lambda params pre (visitExprs f body) multi
        If (IfClause c b) elseIfs els ->
            Fix $ LocatedExpression $ A region $ If
                (IfClause (fmap (visitExprs f) c) (fmap (visitExprs f) b))
                (fmap (fmap (\(IfClause c' b') -> IfClause (fmap (visitExprs f) c') (fmap (visitExprs f) b'))) elseIfs)
                (fmap (visitExprs f) els)
        Let decls pre body ->
            Fix $ LocatedExpression $ A region $ Let (fmap (visitLetDeclaration f) decls) pre body
        Case (e, b) branches ->
            Fix $ LocatedExpression $ A region $ Case (fmap (visitExprs f) e, b) (fmap (fmap $ fmap $ visitExprs f) branches)
        GLShader _ -> expr


transform' ::
    UpgradeDefinition
    -> ImportInfo
    -> Expr -> Expr
transform' (UpgradeDefinition basicsReplacements _ _) importInfo (Fix (LocatedExpression expr)) =
    let
        exposed = ImportInfo._exposed importInfo
        importAliases = ImportInfo._aliases importInfo

        replace var =
            case var of
                VarRef [] name ->
                    Dict.lookup ([UppercaseIdentifier "Basics"], name) basicsReplacements

                VarRef ns name ->
                    let
                        resolvedNs =
                            Maybe.fromMaybe ns $
                            case ns of
                                [single] -> Bimap.lookupR single importAliases
                                _ -> Nothing
                    in
                    Dict.lookup (resolvedNs, name) basicsReplacements

                OpRef (SymbolIdentifier "!") ->
                    Just $
                    Lambda
                      [makeArg "model", makeArg "cmds"] []
                      (Fix $ LocatedExpression $ noRegion $ Binops
                          (makeVarRef "model")
                          [BinopsClause [] var [] (makeVarRef "cmds")]
                          False
                      )
                      False

                OpRef (SymbolIdentifier "%") ->
                    Just $
                    Lambda
                      [makeArg "dividend", makeArg "modulus"] []
                      (Fix $ LocatedExpression $ noRegion $ App
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
                    (Fix $ LocatedExpression $ noRegion $ AST.Expression.Tuple (fmap (\v -> Commented [] (makeVarRef v) []) vars) False)
                    False
    in
    case RA.drop expr of
        VarExpr var ->
            Fix $ LocatedExpression $
            Maybe.fromMaybe expr $ fmap noRegion $ replace var

        App (Fix (LocatedExpression (A _ (VarExpr var)))) args multiline ->
            Maybe.fromMaybe (Fix $ LocatedExpression expr) $ fmap (\new -> applyLambda (Fix $ LocatedExpression $ noRegion new) args multiline) $ replace var

        TupleFunction n ->
            Fix $ LocatedExpression $
            noRegion $ makeTuple n

        App (Fix (LocatedExpression (A _ (TupleFunction n)))) args multiline ->
            applyLambda (Fix $ LocatedExpression $ noRegion $ makeTuple n) args multiline

        ExplicitList terms' trailing multiline ->
            let
                ha = (fmap UppercaseIdentifier ["Html", "Attributes"])
                styleExposed = Dict.lookup (LowercaseIdentifier "style") exposed == Just ha
                haAlias = Bimap.lookup ha importAliases
            in
            Fix $ LocatedExpression $
            noRegion $ ExplicitList (concat $ fmap (expandHtmlStyle styleExposed haAlias) $ terms') trailing multiline

        _ ->
            Fix $ LocatedExpression $
            expr


expandHtmlStyle :: Bool -> Maybe UppercaseIdentifier -> (Comments, PreCommented (WithEol Expr)) -> [(Comments, PreCommented (WithEol Expr))]
expandHtmlStyle styleExposed importAlias (preComma, (pre, WithEol term eol)) =
    let
        lambda fRef =
            Lambda
                [([], noRegion $ AST.Pattern.Tuple [makeArg' "a", makeArg' "b"]) ] []
                (Fix $ LocatedExpression $ noRegion $ App
                    (Fix $ LocatedExpression $ noRegion $ VarExpr $ fRef)
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
    case RA.drop $ (\(Fix (LocatedExpression e)) -> e) term of
        App (Fix (LocatedExpression (A _ (VarExpr var)))) [(preStyle, Fix (LocatedExpression (A _ (ExplicitList styles trailing _))))] _ | isHtmlAttributesStyle var ->
            fmap (\(preComma', (pre', WithEol style eol')) -> (preComma ++ preComma', (pre ++ preStyle ++ pre' ++ trailing ++ (Maybe.maybeToList $ fmap LineComment eol), WithEol (applyLambda (Fix $ LocatedExpression $ noRegion $ lambda var) [([], style)] (FAJoinFirst JoinAll)) eol'))) styles

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
    Fix $ LocatedExpression $ noRegion $ VarExpr $ VarRef [] $ LowercaseIdentifier varName


inlineVar :: LowercaseIdentifier -> Bool -> Expr' -> Expr' -> Expr'
inlineVar name insertMultiline value expr =
    Maybe.fromMaybe expr $ inlineVar' name insertMultiline value expr


inlineVar' :: LowercaseIdentifier -> Bool -> Expr' -> Expr' -> Maybe Expr'
inlineVar' name insertMultiline value expr =
    case expr of
        VarExpr (VarRef [] n) | n == name -> Just value

        AST.Expression.Tuple terms' multiline ->
            let
                step (acc, expand) t@(Commented pre (Fix (LocatedExpression (A _ term))) post) =
                    case inlineVar' name insertMultiline value term of
                        Nothing -> (ReversedList.push t acc, expand)
                        Just term' -> (ReversedList.push (Commented pre (Fix $ LocatedExpression $ noRegion term') post) acc, insertMultiline || expand)

                (terms'', multiline'') = foldl step (ReversedList.empty, multiline) terms'
            in
            Just $ AST.Expression.Tuple (ReversedList.toList terms'') multiline''

        -- TODO: handle expanding multiline in contexts other than tuples

        AST.Expression.Case (Commented pre (Fix (LocatedExpression (A tRegion term))) post, _) branches ->
            case inlineVar' name insertMultiline value term of
                Nothing -> Nothing
                Just term' ->
                    let
                        makeBranch (Commented prePattern p1 postPattern, (_, b1)) =
                            ((prePattern, p1), b1)
                    in
                    -- TODO: matching branches besides the first is not tested
                    Just $ destructureFirstMatch (pre, Fix $ LocatedExpression $ A tRegion term')
                        (fmap makeBranch branches)
                        (AST.Expression.Case (Commented pre (Fix $ LocatedExpression $ noRegion term') post, False) branches)

        AST.Expression.If (IfClause (Commented preCond (Fix (LocatedExpression (A _ cond))) postCond) (Commented preIf ifBody postIf)) [] (preElse, elseBody) ->
            case inlineVar' name insertMultiline value cond of
                Nothing -> Nothing
                Just cond' ->
                    Just $ destructureFirstMatch (preCond, Fix $ LocatedExpression $ noRegion cond')
                        [ (([], noRegion $ AST.Pattern.Literal $ Boolean True), ifBody) -- TODO: not tested
                        , (([], noRegion $ AST.Pattern.Literal $ Boolean False), elseBody)
                        ]
                        (AST.Expression.If (IfClause (Commented preCond (Fix $ LocatedExpression $ noRegion cond') postCond) (Commented preIf ifBody postIf)) [] (preElse, elseBody))

        _ -> Just $ mapExpr (inlineVar name insertMultiline value) expr


destructureFirstMatch :: PreCommented Expr -> [ (PreCommented Pattern, Expr) ] -> Expr' -> Expr'
destructureFirstMatch _ [] fallback = fallback
destructureFirstMatch value ((pat, body):rest) fallback =
    case destructure pat value of
        Just mappings ->
            RA.drop $ (\(Fix (LocatedExpression e)) -> e) $ applyMappings False mappings body

        Nothing ->
            destructureFirstMatch value rest fallback


{-| Returns `Nothing` if the pattern doesn't match, or `Just` with a list of bound variables if the pattern does match. -}
destructure :: PreCommented Pattern -> PreCommented Expr -> Maybe [(LowercaseIdentifier, Expr')]
destructure pat arg =
    case (pat, fmap (\(Fix (LocatedExpression e)) -> e) arg) of
        -- Wildcard `_` pattern
        ( (_, A _ Anything), _ ) -> Just [] -- TODO: not tested

        -- Literals
        ( (preVar, A _ (AST.Pattern.Literal pat))
          , (preArg, A _ (AST.Expression.Literal val))
          )
          | pat == val
          ->
            Just []

        -- Custom type variants with no arguments
        ( (preVar, A _ (Data name []))
          , (preArg, A _ (VarExpr (TagRef ns tag)))
          )
          | name == (ns ++ [tag])
          ->
            Just []

        ( (preVar, A _ (Data name argVars))
          , (preArg, A _ (App (Fix (LocatedExpression (A _ (VarExpr (TagRef ns tag))))) argValues _))
          )
          | name == (ns ++ [tag])
          ->
            concat <$> zipWithM destructure argVars argValues

        -- Named variable pattern
        ( (preVar, A _ (VarPattern name))
          , (preArg, arg')
          ) ->
            Just [(name, Parens $ Commented (preVar ++ preArg) (Fix $ LocatedExpression arg') [])]

        -- Tuple with two elements (TODO: generalize this for all tuples)
        ( (preVar, A _ (AST.Pattern.Tuple [Commented preA (A _ (VarPattern nameA)) postA, Commented preB (A _ (VarPattern nameB)) postB]))
          , (preArg, A _ (AST.Expression.Tuple [Commented preAe eA postAe, Commented preBe eB postBe] _))
          ) ->
            Just
                [ (nameA, Parens $ Commented (preVar ++ preArg) (Fix $ LocatedExpression $ noRegion $ Parens $ Commented (preA ++ preAe) eA (postAe ++ postA)) [])
                , (nameB, Parens $ Commented (preB ++ preBe) eB (postBe ++ postB))
                ]

        -- Record destructuring
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
                        |> fmap (\(Pair (k, _) (_, (Fix (LocatedExpression (A _ v)))) _) -> (k, v))
                        |> Dict.fromList

                fieldMapping :: Commented LowercaseIdentifier -> Maybe (LowercaseIdentifier, Expr')
                fieldMapping (Commented _ var _) =
                    (,) var <$> Dict.lookup var args
            in
            sequence $ fmap fieldMapping varFields

        -- `as`
        ( (preVar, A _ (AST.Pattern.Alias (p, _) (_, varName)))
          , _
          ) ->
            fmap concat $ sequence
                [ destructure (preVar, noRegion $ VarPattern varName) arg
                , destructure ([], p) arg
                ]

        -- TODO: handle other patterns

        _ ->
            Nothing


applyMappings :: Bool -> [(LowercaseIdentifier, Expr')] -> Expr -> Expr
applyMappings insertMultiline mappings body =
    foldl (\e (name, value) -> mapExpr (inlineVar name insertMultiline value) e) body mappings


applyLambda :: Expr -> [PreCommented Expr] -> FunctionApplicationMultiline -> Expr
applyLambda lambda args appMultiline =
    case (RA.drop $ (\(Fix (LocatedExpression e)) -> e) lambda, args) of
        (Lambda (pat:restVar) preBody body multiline, arg:restArgs) ->
            case destructure pat arg of
                Nothing ->
                    -- failed to destructure the next argument, so stop
                    Fix $ LocatedExpression $ noRegion $ App lambda args appMultiline

                Just mappings ->
                    let
                        newBody = applyMappings (appMultiline == FASplitFirst) mappings body

                        newMultiline =
                            case appMultiline of
                                FASplitFirst -> FASplitFirst
                                FAJoinFirst SplitAll -> FASplitFirst
                                FAJoinFirst JoinAll -> FAJoinFirst JoinAll
                    in
                    case restVar of
                        [] ->
                            -- we applied the argument and none are left, so remove the lambda
                            Fix $ LocatedExpression $ noRegion $ App
                                (Fix $ LocatedExpression $ noRegion $ Parens $ Commented preBody newBody [])
                                restArgs
                                newMultiline

                        _:_ ->
                            -- we applied this argument; try to apply the next argument
                            applyLambda (Fix $ LocatedExpression $ noRegion $ Lambda restVar preBody newBody multiline) restArgs newMultiline

        (_, []) -> lambda

        _ -> Fix $ LocatedExpression $ noRegion $ App lambda args appMultiline
