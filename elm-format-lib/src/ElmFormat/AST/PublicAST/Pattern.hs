{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ElmFormat.AST.PublicAST.Pattern (Pattern(..), mkListPattern) where

import ElmFormat.AST.PublicAST.Core
import ElmFormat.AST.PublicAST.Reference
import qualified AST.V0_16 as AST
import qualified Data.Either as Either
import qualified ElmFormat.AST.PublicAST.Core as Core
import qualified ElmVersion
import qualified Parse.Pattern


data Pattern
    = AnythingPattern
    | UnitPattern
    | LiteralPattern LiteralValue
    | VariablePattern VariableDefinition
    | DataPattern
        { constructor :: Reference
        , arguments :: List (LocatedIfRequested Pattern) -- Non-empty
        }
    | TuplePattern
        { terms :: List (LocatedIfRequested Pattern) -- At least two items
        }
    | ListPattern -- Construct with mkListPattern
        { prefix :: List (LocatedIfRequested Pattern)
        , rest :: Maybe (LocatedIfRequested Pattern) -- Must not be a ListPattern
        }
    | RecordPattern
        { fields :: List VariableDefinition
        }
    | PatternAlias
        { alias :: VariableDefinition
        , pattern :: LocatedIfRequested Pattern
        }

mkListPattern :: List (LocatedIfRequested Pattern) -> Maybe (LocatedIfRequested Pattern) -> Pattern
mkListPattern prefix rest =
    case fmap extract rest of
        Just (ListPattern prefix2 rest2) ->
            ListPattern (prefix ++ prefix2) rest2

        _ ->
            ListPattern prefix rest

instance ToPublicAST 'PatternNK where
    type PublicAST 'PatternNK = Pattern

    fromRawAST' config = \case
        AST.Anything ->
            AnythingPattern

        AST.UnitPattern comments ->
            UnitPattern

        AST.LiteralPattern lit ->
            LiteralPattern lit

        AST.VarPattern name ->
            VariablePattern $ VariableDefinition name

        AST.OpPattern _ ->
            error "PublicAST: OpPattern is not supported in Elm 0.19"

        AST.DataPattern (namespace, tag) args ->
            DataPattern
                (mkReference $ TagRef namespace tag)
                (fromRawAST config . (\(C comments a) -> a) <$> args)

        AST.PatternParens (C (pre, post) pat) ->
            extract $ fromRawAST config pat

        AST.TuplePattern terms ->
            TuplePattern
                (fromRawAST config . (\(C (c1, c2) a) -> a) <$> terms)

        AST.EmptyListPattern comments ->
            mkListPattern [] Nothing

        AST.ListPattern terms ->
            mkListPattern
                (fmap (fromRawAST config . (\(C comments a) -> a)) terms)
                Nothing

        AST.ConsPattern (C firstEol first) rest ->
            let
                first' = fromRawAST config first
                rest' = fmap (fromRawAST config . (\(C comments a) -> a)) (AST.toCommentedList rest)
            in
            case reverse rest' of
                [] -> mkListPattern [] (Just first')
                last : mid -> mkListPattern (first' : reverse mid) (Just last)

        AST.EmptyRecordPattern comment ->
            RecordPattern []

        AST.RecordPattern fields ->
            RecordPattern
                (VariableDefinition . (\(C comments a) -> a) <$> fields)

        AST.Alias (C comments1 pat) (C comments2 name) ->
            PatternAlias
                (VariableDefinition name)
                (fromRawAST config pat)

instance FromPublicAST 'PatternNK where
    toRawAST' = \case
        AnythingPattern ->
            AST.Anything

        UnitPattern ->
            AST.UnitPattern []

        LiteralPattern lit ->
            AST.LiteralPattern lit

        VariablePattern (VariableDefinition name) ->
            AST.VarPattern name

        DataPattern constructor arguments ->
            case toRef constructor of
                TagRef ns tag ->
                    AST.DataPattern
                        (ns, tag)
                        (C [] . toRawAST <$> arguments)

                ref ->
                    error ("invalid DataPattern constructor: " <> show ref)

        TuplePattern terms ->
            AST.TuplePattern
                (C ([], []) . toRawAST <$> terms)

        ListPattern [] Nothing ->
            AST.EmptyListPattern []

        ListPattern some Nothing ->
            AST.ListPattern
                (C ([], []) . toRawAST <$> some)

        ListPattern prefix (Just rest) ->
            done $ foldr step (toRawAST rest, []) (toRawAST <$> prefix)
            where
                step next (first, rest) =
                    (next, first : rest)

                done (first, rest) =
                    AST.ConsPattern
                        (C Nothing first)
                        (Either.fromRight undefined $ AST.fromCommentedList $ C ([], [], Nothing) <$> rest)

        RecordPattern [] ->
            AST.EmptyRecordPattern []

        RecordPattern some ->
            AST.RecordPattern
                (C ([], []) . Core.name <$> some)

        PatternAlias alias pattern  ->
            AST.Alias
                (C [] $ toRawAST pattern)
                (C [] $ Core.name alias)


instance ToJSON Pattern where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Pattern where
    toPairs = \case
        AnythingPattern ->
            mconcat
                [ type_ "AnythingPattern"
                ]

        UnitPattern ->
            mconcat
                [ type_ "UnitPattern"
                ]

        LiteralPattern lit ->
            toPairs lit

        VariablePattern def ->
            toPairs def

        DataPattern constructor arguments ->
            mconcat
                [ type_ "DataPattern"
                , "constructor" .= constructor
                , "arguments" .= arguments
                ]

        TuplePattern terms ->
            mconcat
                [ type_ "TuplePattern"
                , "terms" .= terms
                ]

        ListPattern prefix rest ->
            mconcat
                [ type_ "ListPattern"
                , "prefix" .= prefix
                , "rest" .= rest
                ]

        RecordPattern fields ->
            mconcat
                [ type_ "RecordPattern"
                , "fields" .= fields
                ]

        PatternAlias alias pat ->
            mconcat
                [ type_ "PatternAlias"
                , "alias" .= alias
                , "pattern" .= pat
                ]

instance FromJSON Pattern where
    parseJSON = withObjectOrParseString "Pattern" (Parse.Pattern.expr ElmVersion.Elm_0_19) fromRawAST $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "AnythingPattern" ->
                return AnythingPattern

            "UnitPattern" ->
                return UnitPattern

            "IntLiteral" ->
                LiteralPattern <$> parseJSON (Object obj)

            "FloatLiteral" ->
                LiteralPattern <$> parseJSON (Object obj)

            "StringLiteral" ->
                LiteralPattern <$> parseJSON (Object obj)

            "CharLiteral" ->
                LiteralPattern <$> parseJSON (Object obj)

            "VariableDefinition" ->
                VariablePattern <$> parseJSON (Object obj)

            "DataPattern" ->
                DataPattern
                    <$> obj .: "constructor"
                    <*> obj .: "arguments"

            "TuplePattern" ->
                TuplePattern
                    <$> obj .: "terms"

            "ListPattern" ->
                ListPattern
                    <$> obj .: "prefix"
                    <*> obj .: "rest"

            "RecordPattern" ->
                RecordPattern
                    <$> obj .: "fields"

            "PatternAlias" ->
                PatternAlias
                    <$> obj .: "alias"
                    <*> obj .: "pattern"

            _ ->
                fail ("unexpected Pattern tag: " <> tag)
