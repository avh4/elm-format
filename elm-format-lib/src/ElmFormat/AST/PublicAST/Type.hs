{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ElmFormat.AST.PublicAST.Type (Type_(..), CustomTypeVariant(..), mkCustomTypeVariant, fromCustomTypeVariant) where

import ElmFormat.AST.PublicAST.Core
import qualified AST.V0_16 as AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Indexed as I
import qualified Data.ReversedList as ReversedList
import Data.ReversedList (Reversed)
import qualified Data.Either as Either
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty)


data Type_
    = UnitType
    | TypeReference
        { name_tr :: UppercaseIdentifier
        , module_ :: ModuleName
        , arguments :: List (LocatedIfRequested Type_)
        }
    | TypeVariable
        { name_tv :: LowercaseIdentifier
        }
    | TupleType
        { terms :: NonEmpty (LocatedIfRequested Type_) -- At least two items
        }
    | RecordType
        { base :: Maybe LowercaseIdentifier
        , fields :: Map LowercaseIdentifier (LocatedIfRequested Type_) -- Cannot be empty if base is present
        , display :: RecordDisplay
        }
    | FunctionType
        { returnType :: LocatedIfRequested Type_
        , argumentTypes :: List (LocatedIfRequested Type_) -- Non-empty
        }

instance ToPublicAST 'TypeNK where
    type PublicAST 'TypeNK = Type_

    fromRawAST' config = \case
        AST.UnitType comments ->
            UnitType

        AST.TypeConstruction (AST.NamedConstructor ( namespace, name )) args forceMultine ->
            TypeReference
                name
                (ModuleName namespace)
                (fmap (\(C comments a) -> fromRawAST config a) args)

        AST.TypeConstruction (AST.TupleConstructor _) _ _ ->
            error "TODO"

        AST.TypeVariable name ->
            TypeVariable name

        AST.TypeParens (C comments t) ->
            fromRawAST' config (extract $ I.unFix2 t)

        AST.TupleType terms multiline ->
            TupleType
                (fmap (\(C comments a) -> fromRawAST config a) terms)

        AST.RecordType base fields comments multiline ->
            RecordType
                (fmap (\(C comments a) -> a) base)
                (Map.fromList $ (\(C cp (Pair (C ck key) (C cv value) ml)) -> (key, fromRawAST config value)) <$> AST.toCommentedList fields)
                $ RecordDisplay
                    (extract . _key . extract <$> AST.toCommentedList fields)

        AST.FunctionType first rest multiline ->
            case firstRestToRestLast first (AST.toCommentedList rest) of
                (args, C comments last) ->
                    FunctionType
                        (fromRawAST config last)
                        (fmap (\(C comments a) -> fromRawAST config a) args)
        where
            firstRestToRestLast :: AST.C0Eol x -> List (AST.C2Eol a b x) -> (List (AST.C2Eol a b x), AST.C0Eol x)
            firstRestToRestLast first rest =
                done $ foldl (flip step) (ReversedList.empty, first) rest
                where
                    step :: AST.C2Eol a b x -> (Reversed (AST.C2Eol a b x), AST.C0Eol x) -> (Reversed (AST.C2Eol a b x), AST.C0Eol x)
                    step (C (a, b, dn) next) (acc, C dn' last) =
                        (ReversedList.push (C (a, b, dn') last) acc, C dn next)

                    done :: (Reversed (AST.C2Eol a b x), AST.C0Eol x) -> (List (AST.C2Eol a b x), AST.C0Eol x)
                    done (acc, last) =
                        (ReversedList.toList acc, last)

instance FromPublicAST 'TypeNK where
    toRawAST' = \case
        UnitType ->
            AST.UnitType []

        TypeReference name (ModuleName namespace) args ->
            AST.TypeConstruction
                (AST.NamedConstructor ( namespace, name ))
                (C [] . toRawAST <$> args)
                (AST.ForceMultiline False)

        TypeVariable name ->
            AST.TypeVariable name

        TupleType terms ->
            AST.TupleType
                (C ([], [], Nothing) . toRawAST <$> terms)
                (AST.ForceMultiline False)

        RecordType base fields display ->
            AST.RecordType
                (C ([], []) <$> base)
                (Either.fromRight undefined $ AST.fromCommentedList ((\(key, value) -> C ([], [], Nothing) $ Pair (C [] key) (C [] $ toRawAST value) (AST.ForceMultiline False)) <$> Map.toList fields))
                []
                (AST.ForceMultiline True)

        FunctionType returnType argumentTypes ->
            case argumentTypes ++ [ returnType ] of
                first : rest ->
                    AST.FunctionType
                        (C Nothing $ toRawAST first)
                        (Either.fromRight undefined $ AST.fromCommentedList $ fmap (C ([], [], Nothing) . toRawAST) rest)
                        (AST.ForceMultiline False)

                [] ->
                    undefined

instance ToJSON Type_ where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Type_ where
    toPairs = \case
        UnitType ->
            mconcat
                [ type_ "UnitType"
                ]

        TypeReference name module_ arguments ->
            mconcat
                [ type_ "TypeReference"
                , "name" .= name
                , "module" .= module_
                , "arguments" .= arguments
                ]

        TypeVariable name ->
            mconcat
                [ type_ "TypeVariable"
                , "name" .= name
                ]

        TupleType terms ->
            mconcat
                [ type_ "TupleType"
                , "terms" .= terms
                ]

        RecordType Nothing fields display ->
            mconcat
                [ type_ "RecordType"
                , "fields" .= fields
                , "display" .= display
                ]

        RecordType (Just base) fields display ->
            mconcat
                [ type_ "RecordTypeExtension"
                , "base" .= base
                , "fields" .= fields
                , "display" .= display
                ]

        FunctionType returnType argumentTypes ->
            mconcat
                [ type_ "FunctionType"
                , "returnType" .= returnType
                , "argumentTypes" .= argumentTypes
                ]

instance FromJSON Type_ where
    parseJSON = withObject "Type" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "UnitType" ->
                return UnitType

            "TypeReference" ->
                TypeReference
                    <$> obj .: "name"
                    <*> (fromMaybe (ModuleName []) <$> obj .:? "module")
                    <*> obj .:? "arguments" .!= []

            "TypeVariable" ->
                TypeVariable
                    <$> obj .: "name"

            "TupleType" ->
                TupleType
                    <$> obj .: "terms"

            "RecordType" ->
                RecordType Nothing
                    <$> obj .: "fields"
                    <*> return (RecordDisplay [])

            "RecordTypeExtension" ->
                RecordType
                    <$> (Just <$> obj .: "base")
                    <*> obj .: "fields"
                    <*> return (RecordDisplay [])

            "FunctionType" ->
                FunctionType
                    <$> obj .: "returnType"
                    <*> obj .: "argumentTypes"

            _ ->
                fail ("unexpected Type tag: \"" <> tag <> "\"")


data CustomTypeVariant
    = CustomTypeVariant
        { name :: UppercaseIdentifier
        , parameterTypes :: List (LocatedIfRequested Type_)
        }
    deriving (Generic)

mkCustomTypeVariant :: Config -> AST.NameWithArgs UppercaseIdentifier (ASTNS2 Located [UppercaseIdentifier] 'TypeNK) -> CustomTypeVariant
mkCustomTypeVariant config (AST.NameWithArgs name args) =
    CustomTypeVariant
        name
        ((\(C c a) -> fromRawAST config a) <$> args)

fromCustomTypeVariant :: CustomTypeVariant -> AST.NameWithArgs UppercaseIdentifier (ASTNS2 Identity [UppercaseIdentifier] 'TypeNK)
fromCustomTypeVariant = \case
    CustomTypeVariant name parameterTypes ->
        AST.NameWithArgs
            name
            (C [] . toRawAST <$> parameterTypes)

instance ToJSON CustomTypeVariant where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CustomTypeVariant where
    parseJSON = withObject "CustomTypeVariant" $ \obj ->
        CustomTypeVariant
            <$> obj .: "name"
            <*> obj .:? "parameterTypes" .!= []
