{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module ElmFormat.Upgrade_0_19 (UpgradeDefinition, transform, parseUpgradeDefinition, transformModule, mergeUpgradeImports, MatchedNamespace(..)) where

import Elm.Utils ((|>))

import AST.Annotated (updateNamespace)
import AST.V0_16
import AST.Declaration (Declaration(..), TopLevelStructure(..))
import AST.Expression
import AST.Module (Module(Module), ImportMethod)
import AST.Pattern
import AST.Variable
import Control.Applicative ((<|>), liftA2)
import Control.Monad (zipWithM)
import Data.Fix
import ElmFormat.ImportInfo (ImportInfo)
import ElmFormat.Mapping
import ElmVersion
import Reporting.Annotation (Annotated(A))

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


elm0_19upgrade :: Text.Text
elm0_19upgrade = Text.pack $ unlines
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
        { _replacements ::
            Dict.Map
                ([UppercaseIdentifier], String)
                (Fix (Expression (MatchedNamespace [UppercaseIdentifier])))
        , _typeReplacements ::
            Dict.Map
                ([UppercaseIdentifier], UppercaseIdentifier)
                ([LowercaseIdentifier], Type' (MatchedNamespace [UppercaseIdentifier]))
        , _imports :: Dict.Map [UppercaseIdentifier] (Comments, ImportMethod)
        }
    deriving Show


parseUpgradeDefinition :: Text.Text -> Either () UpgradeDefinition
parseUpgradeDefinition definitionText =
    case ElmFormat.Parse.parse Elm_0_19 definitionText of
        Result.Result _ (Result.Ok modu@(Module _ _ _ (_, imports) body)) ->
            let
                importInfo = ImportInfo.fromModule modu

                makeName :: String -> Maybe ([UppercaseIdentifier], String)
                makeName name =
                    (\rev -> (UppercaseIdentifier <$> reverse (tail rev), head rev))
                        <$> reverse <$> splitOn '_' <$> List.stripPrefix "upgrade_" name

                makeTypeName :: UppercaseIdentifier -> Maybe ([UppercaseIdentifier], UppercaseIdentifier)
                makeTypeName (UppercaseIdentifier name) =
                    (\rev -> (UppercaseIdentifier <$> reverse (tail rev), UppercaseIdentifier $ head rev))
                        <$> reverse <$> splitOn '_' <$> List.stripPrefix "Upgrade_" name

                getExpressionReplacement def =
                    case def of
                        Entry (A _ (Definition (A _ (VarPattern (LowercaseIdentifier name))) [] _ upgradeBody)) ->
                            case makeName name of
                                Just functionName -> Just (functionName, stripAnnotation upgradeBody)
                                Nothing -> Nothing

                        Entry (A _ (Definition (A _ (VarPattern (LowercaseIdentifier name))) args comments upgradeBody)) ->
                            case makeName name of
                                Just functionName ->
                                    Just
                                        ( functionName
                                        , Fix $ Lambda args comments (stripAnnotation upgradeBody) False
                                        )

                                Nothing -> Nothing

                        _ ->
                            Nothing

                matchReferences' =
                    matchReferences
                        (Bimap.toMap $ ImportInfo._aliases importInfo)
                        (ImportInfo._directImports importInfo)

                getTypeReplacement = \case
                    Entry (A _ (TypeAlias comments (Commented _ (name, args) _) (_, A _ typ))) ->
                        case makeTypeName name of
                            Just typeName ->
                                Just (typeName, (fmap snd args, fmap matchReferences' typ))

                            Nothing -> Nothing

                    _ -> Nothing
            in
            Right $ UpgradeDefinition
                { _replacements = fmap (mapNamespace matchReferences') $ Dict.fromList $ Maybe.mapMaybe getExpressionReplacement body
                , _typeReplacements = Dict.fromList $ Maybe.mapMaybe getTypeReplacement body
                , _imports = imports
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


transform :: ImportInfo -> Fix (Expression [UppercaseIdentifier]) -> Fix (Expression [UppercaseIdentifier])
transform importInfo =
    case parseUpgradeDefinition elm0_19upgrade of
        Right replacements ->
            mapNamespace (applyReferences (Bimap.toMapR $ ImportInfo._aliases importInfo))
                . transform' replacements importInfo
                . mapNamespace (matchReferences (Bimap.toMap $ ImportInfo._aliases importInfo) (ImportInfo._directImports importInfo))


        Left () ->
            error "Couldn't parse upgrade definition"


transformModule :: UpgradeDefinition -> Module -> Module
transformModule upgradeDefinition modu@(Module a b c (preImports, originalImports) originalBody') =
    let
        importInfo =
            -- Note: this is the info used for matching references in the
            -- source file being transformed, and should NOT include
            -- the imports merged in from the upgrade definition
            ImportInfo.fromModule modu

        transformTopLevelStructure structure =
            case structure of
                Entry (A region (Definition name args comments expr)) ->
                    Entry (A region (Definition name args comments $ addAnnotation noRegion' $ transform' upgradeDefinition importInfo $ stripAnnotation expr))

                Entry (A region (TypeAnnotation name (pre, typ))) ->
                    Entry (A region (TypeAnnotation name (pre, transformType upgradeDefinition typ)))

                _ -> structure

        expressionFromTopLevelStructure structure =
            case structure of
                Entry (A _ (Definition _ _ _ expr)) -> Just expr
                _ -> Nothing

        namespacesWithReplacements =
              Set.fromList $ fmap fst $ Dict.keys $ _replacements upgradeDefinition

        usages body =
            let
                collectExprs = Maybe.mapMaybe expressionFromTopLevelStructure body

                usages' =
                    Dict.unionsWith (Dict.unionWith (+)) $
                    fmap (cata countUsages . stripAnnotation) collectExprs
            in
            fmap (Dict.foldr (+) 0) usages'

        originalBody =
            originalBody'
                |> fmap (updateNamespace (matchReferences (Bimap.toMap $ ImportInfo._aliases importInfo) (ImportInfo._directImports importInfo)))

        finalBody =
            fmap transformTopLevelStructure originalBody

        finalImports =
            mergeUpgradeImports
                originalImports
                (_imports upgradeDefinition)
                namespacesWithReplacements
                (usages finalBody)

        finalImportInfo =
            ImportInfo.fromImports $ fmap snd finalImports
    in
    finalBody
        |> fmap (updateNamespace (applyReferences (Bimap.toMapR $ ImportInfo._aliases finalImportInfo)))
        |> Module a b c (preImports, finalImports)


mergeUpgradeImports ::
    Dict.Map [UppercaseIdentifier] (Comments, ImportMethod)
    -> Dict.Map [UppercaseIdentifier] (Comments, ImportMethod)
    -> Set.Set [UppercaseIdentifier]
    -> Dict.Map (MatchedNamespace [UppercaseIdentifier]) Int
    -> Dict.Map [UppercaseIdentifier] (Comments, ImportMethod)
mergeUpgradeImports originalImports upgradeImports upgradesAttempted usagesAfter =
    let
        -- uBefore ns = Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport ns) usagesBefore
        uAfter ns = Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport ns) usagesAfter
    in
    Dict.union
        (Dict.filterWithKey (\k _ -> uAfter k > 0 || not (Set.member k upgradesAttempted)) originalImports)
        (Dict.filterWithKey (\k _ -> uAfter k > 0) upgradeImports)


data MatchedNamespace t
    = NoNamespace
    | MatchedImport t
    | Unmatched t
    deriving (Eq, Ord, Show)


matchReferences ::
    Ord t =>
    Dict.Map t [t]
    -> Set.Set [t]
    -> [t]
    -> MatchedNamespace [t]
matchReferences aliases imports ns =
    case ns of
        [] -> NoNamespace
        _ ->
            let
                self =
                    if Set.member ns imports then
                        Just ns
                    else
                        Nothing

                fromAlias =
                    case ns of
                        [single] ->
                            Dict.lookup single aliases
                        _ ->
                            Nothing

                resolved =
                    fromAlias <|> self
            in
            case resolved of
                Nothing -> Unmatched ns
                Just single -> MatchedImport single


applyReferences :: Ord t => Dict.Map [t] t -> MatchedNamespace [t] -> [t]
applyReferences aliases ns' =
    case ns' of
        NoNamespace -> []
        MatchedImport ns -> Maybe.fromMaybe ns $ fmap pure $ Dict.lookup ns aliases
        Unmatched name -> name


data Source
    = FromUpgradeDefinition
    | FromSource


{- An expression annotated with a Source -- this type is used throughout the upgrade transformation. -}
type UExpr = Fix (AnnotatedExpression (MatchedNamespace [UppercaseIdentifier]) Source)


transform' ::
    UpgradeDefinition
    -> ImportInfo
    -> Fix (Expression (MatchedNamespace [UppercaseIdentifier])) -> Fix (Expression (MatchedNamespace [UppercaseIdentifier]))
transform' upgradeDefinition importInfo =
    stripAnnotation . bottomUp (simplify . applyUpgrades upgradeDefinition importInfo) . addAnnotation FromSource


transformType ::
    UpgradeDefinition
    -> Type (MatchedNamespace [UppercaseIdentifier])
    -> Type (MatchedNamespace [UppercaseIdentifier])
transformType upgradeDefinition =
    bottomUpType (transformType' upgradeDefinition)


transformType' ::
    UpgradeDefinition
    -> Type (MatchedNamespace [UppercaseIdentifier])
    -> Type (MatchedNamespace [UppercaseIdentifier])
transformType' upgradeDefinition typ = case typ of
    A region (FunctionType (WithEol (A firstRegion (TypeConstruction (NamedConstructor (MatchedImport ctorNs) ctorName) args)) eol) rest ml) ->
        case Dict.lookup (ctorNs, ctorName) (_typeReplacements upgradeDefinition) of
            Just (argOrder, newTyp) ->
                let
                    inlines =
                        Dict.fromList $ zip argOrder (fmap snd args)

                    first' =
                        inlineTypeVars inlines $ noRegion newTyp

                    rest' =
                        fmap
                            (\(preA, postA, t, eol') -> (preA, postA, transformType upgradeDefinition t, eol'))
                            rest
                in
                A region $ FunctionType (WithEol first' eol) rest' ml

            Nothing -> typ

    _ -> typ


applyUpgrades :: UpgradeDefinition -> ImportInfo -> UExpr -> UExpr
applyUpgrades upgradeDefinition importInfo expr =
    let
        exposed = ImportInfo._exposed importInfo
        replacements = _replacements upgradeDefinition

        replace :: Ref (MatchedNamespace [UppercaseIdentifier]) -> Maybe (Fix (Expression (MatchedNamespace [UppercaseIdentifier])))
        replace var =
            case var of
                VarRef NoNamespace (LowercaseIdentifier name) ->
                    Dict.lookup ([UppercaseIdentifier "Basics"], name) replacements

                VarRef (MatchedImport ns) (LowercaseIdentifier name) ->
                    Dict.lookup (ns, name) replacements

                TagRef (MatchedImport ns) (UppercaseIdentifier name) ->
                    Dict.lookup (ns, name) replacements

                OpRef (SymbolIdentifier "!") ->
                    Just $ Fix $
                    Lambda
                      [makeArg "model", makeArg "cmds"] []
                      (Fix $ Binops
                          (makeVarRef "model")
                          [BinopsClause [] var [] (makeVarRef "cmds")]
                          False
                      )
                      False

                OpRef (SymbolIdentifier "%") ->
                    Just $ Fix $
                    Lambda
                      [makeArg "dividend", makeArg "modulus"] []
                      (Fix $ App
                          (makeVarRef "modBy")
                          [ ([], makeVarRef "modulus")
                          , ([], makeVarRef "dividend")
                          ]
                          (FAJoinFirst JoinAll)
                      )
                      False

                _ -> Nothing

        makeTuple :: Int -> Fix (Expression (MatchedNamespace ns))
        makeTuple n =
            let
                vars =
                  if n <= 26
                    then fmap (\c -> [c]) (take n ['a'..'z'])
                    else error (pleaseReport'' "UNEXPECTED TUPLE" "more than 26 elements")
            in
                Fix $ Lambda
                    (fmap makeArg vars)
                    []
                    (Fix $ AST.Expression.Tuple (fmap (\v -> Commented [] (makeVarRef v) []) vars) False)
                    False
    in
    case unFix expr of
        AE (A _ (VarExpr var)) ->
            Maybe.fromMaybe expr $ fmap (addAnnotation FromUpgradeDefinition) $ replace var

        AE (A _ (TupleFunction n)) ->
            addAnnotation FromUpgradeDefinition $ makeTuple n

        AE (A ann (ExplicitList terms' trailing multiline)) ->
            let
                ha = (fmap UppercaseIdentifier ["Html", "Attributes"])
                styleExposed = Dict.lookup (LowercaseIdentifier "style") exposed == Just ha
            in
            Fix $ AE $ A ann $ ExplicitList (concat $ fmap (expandHtmlStyle styleExposed) $ terms') trailing multiline

        _ ->
            expr


simplify :: UExpr -> UExpr
simplify expr =
    let
        isElmFixRemove (_, (_, WithEol (Fix (AE (A FromUpgradeDefinition (VarExpr (VarRef (MatchedImport [UppercaseIdentifier "ElmFix"]) (LowercaseIdentifier "remove")))))) _))= True
        isElmFixRemove (_, (_, WithEol (Fix (AE (A FromUpgradeDefinition (VarExpr (VarRef (Unmatched [UppercaseIdentifier "ElmFix"]) (LowercaseIdentifier "remove")))))) _))= True
        isElmFixRemove _ = False
    in
    case unFix expr of
        -- apply arguments to special functions (like literal lambdas)
        AE (A source (App fn args multiline)) ->
            simplifyFunctionApplication source fn args multiline

        -- Remove ElmFix.remove from lists
        AE (A source (ExplicitList terms' trailing multiline)) ->
            Fix $ AE $ A source $ ExplicitList
                (filter (not . isElmFixRemove) terms')
                trailing
                multiline

        -- Inline field access of a literal record
        AE (A FromUpgradeDefinition (Access e field)) ->
            case e of
                Fix (AE (A _ (AST.Expression.Record _ fs _ _))) ->
                    case List.find (\(_, (_, WithEol (Pair (f, _) _ _) _)) -> f == field) fs of
                        Nothing ->
                            expr
                        Just (_, (_, WithEol (Pair _ (_, fieldValue) _) _)) ->
                            fieldValue
                _ ->
                    expr

        -- reduce if expressions with a literal bool condition
        AE (A FromUpgradeDefinition (If (IfClause (Commented preCond cond postCond) (Commented preIf ifBody postIf)) [] (preElse, elseBody))) ->
            destructureFirstMatch (preCond, cond)
                [ (([], noRegion $ AST.Pattern.Literal $ Boolean True), ifBody) -- TODO: not tested
                , (([], noRegion $ AST.Pattern.Literal $ Boolean False), elseBody)
                ]
                expr

        -- reduce case expressions
        AE (A FromUpgradeDefinition (Case (Commented pre term post, _) branches)) ->
            let
                makeBranch (Commented prePattern p1 postPattern, (_, b1)) =
                    ((prePattern, p1), b1)
            in
            destructureFirstMatch (pre, term)
                (fmap makeBranch branches)
                expr

        _ ->
            expr


expandHtmlStyle :: Bool -> (Comments, PreCommented (WithEol UExpr)) -> [(Comments, PreCommented (WithEol UExpr))]
expandHtmlStyle styleExposed (preComma, (pre, WithEol term eol)) =
    let
        lambda fRef =
            addAnnotation FromUpgradeDefinition $ Fix $
            Lambda
                [([], noRegion $ AST.Pattern.Tuple [makeArg' "a", makeArg' "b"]) ] []
                (Fix $ App
                    (Fix $ VarExpr $ fRef)
                    [ ([], makeVarRef "a")
                    , ([], makeVarRef "b")
                    ]
                    (FAJoinFirst JoinAll)
                )
                False

        isHtmlAttributesStyle var =
            case var of
                VarRef (MatchedImport [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"]) (LowercaseIdentifier "style") -> True
                VarRef NoNamespace (LowercaseIdentifier "style") -> styleExposed
                _ -> False
    in
    case dropAnnotation term of
        App (Fix (AE (A _ (VarExpr var)))) [(preStyle, Fix (AE (A _ (ExplicitList styles trailing _))))] _
          | isHtmlAttributesStyle var
          ->
            let
                convert (preComma', (pre', WithEol style eol')) =
                    ( preComma ++ preComma'
                    , ( pre++ preStyle ++ pre' ++ trailing ++ (Maybe.maybeToList $ fmap LineComment eol)
                      , WithEol (Fix $ AE $ A FromUpgradeDefinition $ App (lambda var) [([], style)] (FAJoinFirst JoinAll)) eol'
                      )
                    )
            in
            fmap convert styles

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


noRegion' :: Region.Region
noRegion' =
    Region.Region nowhere nowhere


noRegion :: a -> RA.Located a
noRegion =
    RA.at nowhere nowhere


makeArg :: String -> (Comments, Pattern ns)
makeArg varName =
    ([], noRegion $ VarPattern $ LowercaseIdentifier varName)


makeArg' :: String -> Commented (Pattern ns)
makeArg' varName =
    Commented [] (noRegion $ VarPattern $ LowercaseIdentifier varName) []


makeVarRef :: String -> Fix (Expression (MatchedNamespace any))
makeVarRef varName =
    Fix $ VarExpr $ VarRef NoNamespace $ LowercaseIdentifier varName


applyMappings :: Bool -> Dict.Map LowercaseIdentifier UExpr -> UExpr -> UExpr
applyMappings insertMultiline mappings =
    bottomUp simplify
        . mapAnnotation snd
        . bottomUp (inlineVars ((==) NoNamespace) insertMultiline mappings)
        . mapAnnotation ((,) False)


inlineVars ::
    (ns -> Bool)
    -> Bool
    -> Dict.Map LowercaseIdentifier (Fix (AnnotatedExpression ns ann))
    -> Fix (AnnotatedExpression ns (Bool, ann))
    -> Fix (AnnotatedExpression ns (Bool, ann))
inlineVars isLocal insertMultiline mappings expr =
    case unFix expr of
        AE (A _ (VarExpr (VarRef ns n))) | isLocal ns->
            case Dict.lookup n mappings of
                Just (Fix (AE (A ann e))) ->
                    Fix $ AE $ A (insertMultiline, ann) $
                    fmap (mapAnnotation ((,) False)) e

                Nothing ->
                    expr

        AE (A ann (AST.Expression.Tuple terms' multiline)) ->
            let
                requestedMultiline (Commented _ (Fix (AE (A (m, _) _))) _) = m
                newMultiline = multiline || any requestedMultiline terms'
            in
            Fix $ AE $ A ann $ AST.Expression.Tuple terms' newMultiline

        -- TODO: handle expanding multiline in contexts other than tuples

        _ -> expr


inlineTypeVars ::
    Dict.Map LowercaseIdentifier (Type ns)
    -> Type ns
    -> Type ns
inlineTypeVars mappings =
    bottomUpType (inlineTypeVars' mappings)


inlineTypeVars' ::
    Dict.Map LowercaseIdentifier (Type ns)
    -> Type ns
    -> Type ns
inlineTypeVars' mappings (A region typ) =
    case typ of
        TypeVariable ref ->
            case Dict.lookup ref mappings of
                Just replacement -> replacement
                Nothing -> A region typ

        _ -> A region typ


{- Applies a transformation function to a Type from the bottom up. -}
bottomUpType :: (Type ns -> Type ns) -> Type ns -> Type ns
bottomUpType f (A region typ) =
    f $
    case typ of
        UnitType _ -> A region typ
        TypeVariable _ -> A region typ
        TypeConstruction ctor args -> A region $ TypeConstruction ctor (fmap (fmap $ bottomUpType f) args)
        TypeParens t -> A region $ TypeParens (fmap (bottomUpType f) t)
        TupleType ts -> A region $ TupleType (fmap (fmap $ fmap $ bottomUpType f) ts)
        RecordType base fields cs ml -> A region $ RecordType base (fmap (fmap $ fmap $ fmap $ fmap $ bottomUpType f) fields) cs ml
        FunctionType first rest ml -> A region $ FunctionType (fmap (bottomUpType f) first) (fmap (\(a, b, t, e) -> (a, b, bottomUpType f t, e)) rest) ml


destructureFirstMatch :: PreCommented UExpr -> [ (PreCommented (Pattern (MatchedNamespace [UppercaseIdentifier])), UExpr) ] -> UExpr -> UExpr
destructureFirstMatch value choices fallback =
    -- If we find an option that Matches, use the first one we find.
    -- Otherwise, keep track of which ones *could* match -- if there is only one remaining, we can use that
    let
        resolve [] = fallback
        resolve [body] = body
        resolve _ = fallback -- TODO: could instead indicate which options the caller should keep

        destructureFirstMatch' [] othersPossible = resolve othersPossible
        destructureFirstMatch' ((pat, body):rest) othersPossible =
            case destructure pat value of
                Matches mappings ->
                    resolve (applyMappings False mappings body : othersPossible)

                CouldMatch ->
                    destructureFirstMatch' rest (body:othersPossible)

                DoesntMatch ->
                    destructureFirstMatch' rest othersPossible
    in
    destructureFirstMatch' choices []


withComments :: Comments -> UExpr -> Comments -> UExpr
withComments [] e [] = e
withComments pre e post = Fix $ AE $ A FromUpgradeDefinition $ Parens $ Commented pre e post


data DestructureResult a
    = Matches a
    | CouldMatch
    | DoesntMatch
    deriving (Functor, Show)


instance Applicative DestructureResult where
    pure a = Matches a

    liftA2 f (Matches a) (Matches b) = Matches (f a b)
    liftA2 _ (Matches _) CouldMatch = CouldMatch
    liftA2 _ CouldMatch (Matches _) = CouldMatch
    liftA2 _ CouldMatch CouldMatch = CouldMatch
    liftA2 _ _ DoesntMatch = DoesntMatch
    liftA2 _ DoesntMatch _ = DoesntMatch


instance Monad DestructureResult where
    (Matches a) >>= f = f a
    CouldMatch >>= _ = CouldMatch
    DoesntMatch >>= _ = DoesntMatch


{-| Returns `Nothing` if the pattern doesn't match, or `Just` with a list of bound variables if the pattern does match. -}
destructure :: PreCommented (Pattern (MatchedNamespace [UppercaseIdentifier])) -> PreCommented UExpr -> DestructureResult (Dict.Map LowercaseIdentifier UExpr)
destructure pat arg =
    let
        namespaceMatch nsd ns =
            case (nsd, ns) of
                (Unmatched nsd', MatchedImport ns') -> nsd' == ns'
                _ -> nsd == ns
    in
    case (pat, fmap dropAnnotation arg) of
        -- Parens in expression
        ( _
          , (preArg, AST.Expression.Parens (Commented pre inner post))
          )
          ->
            destructure pat (preArg ++ pre ++ post, inner)

        -- Parens in pattern
        ( (preVar, A _ (PatternParens (Commented pre inner post)))
          , _
          )
          ->
            destructure (preVar ++ pre ++ post, inner) arg

        -- Wildcard `_` pattern
        ( (_, A _ Anything), _ ) -> Matches Dict.empty

        -- Unit
        ( (preVar, A _ (UnitPattern _))
          , (preArg, AST.Expression.Unit _)
          )
          ->
            Matches Dict.empty

        -- Literals
        ( (preVar, A _ (AST.Pattern.Literal pat))
          , (preArg, AST.Expression.Literal val)
          )
          ->
            -- TODO: handle ints/strings that are equal but have different representations
            if pat == val then
                Matches Dict.empty
            else
                DoesntMatch

        -- Custom type variants with no arguments
        ( (preVar, A _ (Data nsd name []))
          , (preArg, VarExpr (TagRef ns tag))
          )
          ->
            if name == tag && namespaceMatch nsd ns then
                Matches Dict.empty
            else
                DoesntMatch

        -- Custom type variants with arguments
        ( (preVar, A _ (Data nsd name argVars))
          , (preArg, App (Fix (AE (A _ (VarExpr (TagRef ns tag))))) argValues _)
          )
          ->
            if name == tag && namespaceMatch nsd ns then
                Dict.unions <$> zipWithM destructure argVars argValues
            else
                DoesntMatch

        -- Custom type variants where pattern and value don't match in having args
        ( (_, A _ (Data _ _ (_:_))), (_, VarExpr (TagRef _ _)) ) -> DoesntMatch
        ( (_, A _ (Data _ _ [])), (_, App (Fix (AE (A _ (VarExpr (TagRef _ _))))) _ _) ) -> DoesntMatch

        -- Named variable pattern
        ( (preVar, A _ (VarPattern name))
          , (preArg, arg')
          ) ->
            Matches $ Dict.singleton name (withComments (preVar ++ preArg) (snd arg) [])

        -- `<|` which could be parens in expression
        ( _
          , (preArg, AST.Expression.Binops e (BinopsClause pre (OpRef (SymbolIdentifier "<|")) post arg1 : rest) ml)
          )
          ->
            destructure pat (preArg, Fix $ AE $ A FromSource $ App e [(pre ++ post, Fix $ AE $ A FromSource $ Binops arg1 rest ml)] (FAJoinFirst JoinAll))

        -- Tuple with two elements (TODO: generalize this for all tuples)
        ( (preVar, A _ (AST.Pattern.Tuple [Commented preA (A _ (VarPattern nameA)) postA, Commented preB (A _ (VarPattern nameB)) postB]))
          , (preArg, AST.Expression.Tuple [Commented preAe eA postAe, Commented preBe eB postBe] _)
          ) ->
            Matches $ Dict.fromList
                [ (nameA, withComments (preVar ++ preArg) (withComments (preA ++ preAe) eA (postAe ++ postA)) [])
                , (nameB, withComments (preB ++ preBe) eB (postBe ++ postB))
                ]

        -- Record destructuring
        ( (preVar, A _ (AST.Pattern.Record varFields))
          , (preArg, AST.Expression.Record _ argFields _ _)
          ) ->
            let
                args :: Dict.Map LowercaseIdentifier UExpr
                args =
                    argFields
                        |> fmap snd
                        |> fmap snd
                        |> fmap (\(WithEol a _) -> a)
                        |> fmap (\(Pair (k, _) (_, v) _) -> (k, v))
                        |> Dict.fromList

                fieldMapping :: Commented LowercaseIdentifier -> Maybe (LowercaseIdentifier, UExpr)
                fieldMapping (Commented _ var _) =
                    (,) var <$> Dict.lookup var args
            in
            Maybe.fromMaybe DoesntMatch $ fmap (Matches . Dict.fromList) $ sequence $ fmap fieldMapping varFields

        -- `as`
        ( (preVar, A _ (AST.Pattern.Alias (p, _) (_, varName)))
          , _
          ) ->
            fmap Dict.unions $ sequence
                [ destructure (preVar, noRegion $ VarPattern varName) arg
                , destructure ([], p) arg
                ]

        -- TODO: handle other patterns

        _ ->
            CouldMatch


simplifyFunctionApplication :: Source -> UExpr -> [PreCommented (UExpr)] -> FunctionApplicationMultiline -> UExpr
simplifyFunctionApplication appSource fn args appMultiline =
    case (unFix fn, args) of
        (AE (A lambdaSource (Lambda (pat:restVar) preBody body multiline)), arg:restArgs) ->
            case destructure pat arg of
                Matches mappings ->
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
                            Fix $ AE $ A appSource $ App
                                (withComments preBody newBody [])
                                restArgs
                                newMultiline

                        _:_ ->
                            -- we applied this argument; try to apply the next argument
                            simplifyFunctionApplication appSource (Fix $ AE $ A lambdaSource $ Lambda restVar preBody newBody multiline) restArgs newMultiline
                _ ->
                    -- failed to destructure the next argument, so stop
                    Fix $ AE $ A appSource $ App fn args appMultiline


        (_, []) -> fn

        _ -> Fix $ AE $ A appSource $ App fn args appMultiline
