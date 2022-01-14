{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuantifiedConstraints #-}

module ElmFormat.Normalize (shallow, deepMonad) where

{-| Applies AST normalizations that are considered part
elm-format's "formatting".
-}

import AST.V0_16
import AST.Structure
import qualified Data.Indexed as I
import Control.Monad (join)
import Control.Applicative (liftA2)
import ElmVersion (ElmVersion)
import qualified ElmVersion


{-| Simply uses `join` to combine layers -}
deepMonad :: forall annf nk.
    Traversable annf => Monad annf =>
    ElmVersion
    -> I.Fix2 annf (ASTNS [UppercaseIdentifier]) nk
    -> I.Fix2 annf (ASTNS [UppercaseIdentifier]) nk
deepMonad elmVersion = I.fold2 go
    where
        go :: annf (AST (VariableNamespace [UppercaseIdentifier]) (I.Fix2 annf (ASTNS [UppercaseIdentifier])) i)
         -> I.Fix2 annf (ASTNS [UppercaseIdentifier]) i
        go original =
            I.Fix2 $ join $ fmap (shallow elmVersion) original


-- {-| Will use `normalized <> original` at each layer. -}
-- deepSemigroup :: forall annf ns nk.
--     Traversable annf => Monad annf =>
--     (forall j. Semigroup (annf (AST (VariableNamespace ns) (I.Fix2 annf (ASTNS ns)) j))) =>

--     (forall a. Show a => Show (annf a)) =>
--     Show ns =>

--     I.Fix2 annf (ASTNS ns) nk -> I.Fix2 annf (ASTNS ns) nk
-- deepSemigroup = I.fold2 go
--     where
--         go :: annf (AST (VariableNamespace ns) (I.Fix2 annf (ASTNS ns)) i)
--          -> I.Fix2 annf (ASTNS ns) i
--         go original = trace "\n" $
--             I.Fix2 $ join $ traceShowId $
--             fmap (<> original) $ traceShowId $
--             fmap shallow $ traceShowId $ original


-- {-| Will use `normalized <|> original` at each layer. -}
-- deepAlternative :: forall annf ns nk.
--     (Traversable annf) => Monad annf =>
--     Alternative annf =>
--     (forall j. Show (annf (AST (VariableNamespace ns) (I.Fix2 annf (ASTNS ns)) j))) =>

--     I.Fix2 annf (ASTNS ns) nk -> I.Fix2 annf (ASTNS ns) nk
-- deepAlternative = I.fold2 (go . traceShowId)
--     where
--         go :: annf (AST (VariableNamespace ns) (I.Fix2 annf (ASTNS ns)) i)
--          -> I.Fix2 annf (ASTNS ns) i
--         go original =
--             I.Fix2 $ join $
--             fmap (<|> original) $
--             fmap (shallow) original


-- deep :: forall annf ns nk. (Traversable annf, Monad annf) =>
--     (forall a. (a -> annf a) -> annf a -> annf (annf a)) ->
--     I.Fix2 annf (ASTNS ns) nk -> I.Fix2 annf (ASTNS ns) nk
-- deep merge = I.fold2 go
--     where
--         go :: annf (AST (VariableNamespace ns) (I.Fix2 annf (ASTNS ns)) i)
--          -> I.Fix2 annf (ASTNS ns) i
--         go original =
--             -- I.Fix2 $ maybe _ _ $ _ $ fmap (fromMaybe original) $ fmap sequenceA
--             -- -- fromMaybe original $ sequenceA
--             -- (fmap (getCompose . shallow) original)
--             I.Fix2 $ join $ merge shallow original


shallow ::
    forall annf nk.
    (Traversable annf, Monad annf) =>
    ElmVersion
    -> ASTNS [UppercaseIdentifier] (I.Fix2 annf (ASTNS [UppercaseIdentifier])) nk
    -> annf (ASTNS [UppercaseIdentifier] (I.Fix2 annf (ASTNS [UppercaseIdentifier])) nk)
shallow elmVersion = \case
    App left [] _ ->
        I.unFix2 left

    -- Remove parens in function arguments when the comments can be merged with the surrounding whitespace
    App left args multiline -> do
        -- TODO: This currently joins `annf` for _all_ the args, but ideally we only want to join for ones that we want to simplify and leave the others untouched.
        newArgs <- gg args
        pure $ App left newArgs multiline
        where
            gg ::
                List (C1 'BeforeTerm (I.Fix2 annf (ASTNS ns) 'ExpressionNK))
                -> annf (List (C1 'BeforeTerm (I.Fix2 annf (ASTNS ns) 'ExpressionNK)))
            gg [] = pure []
            gg (next : rest) =
                liftA2 (:) (removeParens next) (gg rest)

            removeParens ::
                C1 'BeforeTerm (I.Fix2 annf (ASTNS ns) 'ExpressionNK)
                -> annf (C1 'BeforeTerm (I.Fix2 annf (ASTNS ns) 'ExpressionNK))
            removeParens (C pre e) =
                fmap (fmap (I.Fix2 . pure) . (\(c, e') -> C (pre ++ c) e'))
                ((>>= matchPreCommentedParens) (I.unFix2 e))

            matchPreCommentedParens ::
                ASTNS1 annf ns 'ExpressionNK
                -> annf (Comments, ASTNS1 annf ns 'ExpressionNK)
            matchPreCommentedParens = \case
                Parens (C (pre', []) e) -> (,) pre' <$> I.unFix2 e
                other -> pure ([], other)

    -- Convert literal range syntax if it's not allowed
    Range left right ->
        if ElmVersion.syntax_0_18_disallowLiteralRange elmVersion
        then
            case (left, right) of
                -- No comments after terms, so we can skip adding parens
                (C (preLeft, []) left', C (preRight, []) right') ->
                    pure $ App
                        (mkVarRef "List" "range")
                        [ C preLeft left'
                        , C preRight right'
                        ]
                        (FAJoinFirst JoinAll)

                _ ->
                    pure $ App
                        (mkVarRef "List" "range")
                        [ C [] $ I.Fix2 $ pure $ Parens left
                        , C [] $ I.Fix2 $ pure $ Parens right
                        ]
                        (FAJoinFirst JoinAll)
        else
            pure $ Range left right

    ast -> pure ast


mkVarRef :: Applicative m => String -> String -> I.Fix2 m (ASTNS [UppercaseIdentifier]) 'ExpressionNK
mkVarRef ns1 name =
    I.Fix2 $ pure $ VarExpr $ I.Fix2 $ pure $ VarRef_ $ VarRef [UppercaseIdentifier ns1] (LowercaseIdentifier name)
