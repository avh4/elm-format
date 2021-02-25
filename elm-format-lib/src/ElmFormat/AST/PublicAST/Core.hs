{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ElmFormat.AST.PublicAST.Core
    ( module Data.Functor.Identity
    , module Data.Aeson
    , module Data.Aeson.Encoding.Internal
    , module GHC.Generics
    , module ElmFormat.AST.Shared
    , module AST.V0_16
    , module AST.Structure
    , module Reporting.Annotation
    , module Reporting.Region
    , module Data.Coapplicative
    , module ElmFormat.AST.PublicAST.MaybeF
    , module ElmFormat.AST.PublicAST.Config
    , ToPairs(..)
    , ToMaybeJSON(..)
    , type_
    , LocatedIfRequested(..)
    , ModuleName(..)
    , FromPublicAST(..)
    , ToPublicAST(..)
    , VariableDefinition(..)
    , RecordDisplay(..)
    ,fromLocated,fromRawAST,toRawAST, noRegion) where

import Data.Functor.Identity
import Data.Aeson
import Data.Aeson.Encoding.Internal (pair)
import GHC.Generics
import ElmFormat.AST.Shared
import AST.V0_16 (NodeKind(..), Pair(..))
import AST.Structure (ASTNS, ASTNS1, mapNs)
import qualified AST.V0_16 as AST
import qualified AST.Module as AST
import qualified AST.Listing as AST
import Data.Indexed as I
import Reporting.Annotation (Located(A))
import qualified Reporting.Annotation
import Reporting.Region (Region)
import qualified Reporting.Region as Region
import Data.Coapplicative
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Aeson.Encoding.Internal as AesonInternal
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import ElmFormat.AST.PublicAST.Config (Config)
import qualified ElmFormat.AST.PublicAST.Config as Config
import ElmFormat.AST.PublicAST.MaybeF
import qualified Data.Aeson as Aeson
import Data.Int (Int64)


class ToPairs a where
    toPairs :: a -> Series


class ToMaybeJSON a where
    toMaybeEncoding :: a -> Maybe Encoding


type_ :: String -> Series
type_ t =
    "tag" .= t


class ToPublicAST (nk :: NodeKind) where
    type PublicAST nk
    fromRawAST' :: Config -> ASTNS1 Located [UppercaseIdentifier] nk -> PublicAST nk

fromRawAST :: ToPublicAST nk => Config -> ASTNS Located [UppercaseIdentifier] nk -> LocatedIfRequested (PublicAST nk)
fromRawAST config =
    fmap (fromRawAST' config) . fromLocated config . I.unFix


class ToPublicAST nk => FromPublicAST (nk :: NodeKind) where
    toRawAST' :: PublicAST nk -> ASTNS1 Identity [UppercaseIdentifier] nk

toRawAST :: FromPublicAST nk => LocatedIfRequested (PublicAST nk) -> ASTNS Identity [UppercaseIdentifier] nk
toRawAST =
    I.Fix . Identity . toRawAST' . extract


--
-- Common types
--


newtype ModuleName =
    ModuleName [UppercaseIdentifier]
    deriving (Eq, Ord)

instance Show ModuleName where
    show (ModuleName ns) = List.intercalate "." $ fmap (\(UppercaseIdentifier v) -> v) ns

instance ToJSON ModuleName where
    toJSON = undefined
    toEncoding (ModuleName []) = toEncoding Null
    toEncoding namespace = toEncoding $ show namespace

instance ToJSONKey ModuleName where
    toJSONKey =
        ToJSONKeyText
            (Text.pack . show)
            (AesonInternal.string . show)

instance FromJSON ModuleName where
    parseJSON = withText "ModuleName" $
        return . ModuleName . fmap (UppercaseIdentifier . Text.unpack) . Text.splitOn "."

instance FromJSONKey ModuleName where
    fromJSONKey = FromJSONKeyText (ModuleName . fmap (UppercaseIdentifier . Text.unpack) . Text.splitOn ".")


newtype VariableDefinition
    = VariableDefinition
        { name :: LowercaseIdentifier
        }

instance ToJSON VariableDefinition where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs VariableDefinition where
    toPairs (VariableDefinition name) =
        mconcat
            [ type_ "VariableDefinition"
            , "name" .= name
            ]

instance FromJSON VariableDefinition where
    parseJSON = withObject "VariableDefinition" $ \obj -> do
        VariableDefinition
            <$> obj .: "name"


newtype RecordDisplay
    = RecordDisplay
        { fieldOrder :: List LowercaseIdentifier
        }
    deriving (Generic)

instance ToJSON RecordDisplay where
    toEncoding = genericToEncoding defaultOptions


newtype LocatedIfRequested a
    = LocatedIfRequested (MaybeF Located a)
    deriving (Functor)

instance Coapplicative LocatedIfRequested where
    extract (LocatedIfRequested a) = extract a

instance Prelude.Foldable LocatedIfRequested where
    foldMap f (LocatedIfRequested a) = Prelude.foldMap f a

instance Traversable LocatedIfRequested where
    traverse f (LocatedIfRequested a) =
        LocatedIfRequested <$> traverse f a

fromLocated :: Config -> Located a -> LocatedIfRequested a
fromLocated config la =
    if Config.showSourceLocation config
        then LocatedIfRequested $ JustF la
        else LocatedIfRequested $ NothingF $ extract la

instance (ToPairs a, ToJSON a) => ToJSON (LocatedIfRequested a) where
    toJSON = undefined
    toEncoding = \case
        LocatedIfRequested (JustF la) -> toEncoding la
        LocatedIfRequested (NothingF a) -> toEncoding a

instance (FromJSON a) => FromJSON (LocatedIfRequested a) where
    parseJSON json =
        LocatedIfRequested . NothingF <$> parseJSON json



--
-- Instances for types defined elsewhere
--


instance ToPairs a => ToJSON (Located a) where
    toJSON = undefined
    toEncoding (A region a) =
        pairs (toPairs a <> "sourceLocation" .= region)


instance ToJSON Region where
    toJSON = undefined
    toEncoding region =
        pairs $ mconcat
            [ "start" .= Region.start region
            , "end" .= Region.end region
            ]


instance ToJSON Region.Position where
    toJSON = undefined
    toEncoding pos =
        pairs $ mconcat
            [ "line" .= Region.line pos
            , "col" .= Region.column pos
            ]


instance ToJSON UppercaseIdentifier where
    toJSON = undefined
    toEncoding (UppercaseIdentifier name) = toEncoding name

instance FromJSON UppercaseIdentifier where
    parseJSON = withText "UppercaseIdentifier" $ \case
        -- XXX: shouldn't crash on empty string
        text | Char.isUpper $ Text.head text ->
            return $ UppercaseIdentifier $ Text.unpack text

        _ ->
            fail "expected a string starting with an uppercase letter"
instance FromJSONKey UppercaseIdentifier where
    fromJSONKey = FromJSONKeyText (UppercaseIdentifier . Text.unpack)


instance ToJSON LowercaseIdentifier where
    toJSON = undefined
    toEncoding (LowercaseIdentifier name) = toEncoding name
instance ToJSONKey LowercaseIdentifier where
    toJSONKey =
        ToJSONKeyText
            (\(LowercaseIdentifier name) -> Text.pack name)
            (\(LowercaseIdentifier name) -> AesonInternal.string name)

instance FromJSON LowercaseIdentifier where
    parseJSON = withText "LowercaseIdentifier" $ \case
        -- XXX: shouldn't crash on empty string
        text | Char.isLower $ Text.head text ->
            return $ LowercaseIdentifier $ Text.unpack text

        _ ->
            fail "expected a string starting with a lowercase letter"
instance FromJSONKey LowercaseIdentifier where
    fromJSONKey = FromJSONKeyText (LowercaseIdentifier . Text.unpack)


instance ToJSON SymbolIdentifier where
    toJSON = undefined
    toEncoding (SymbolIdentifier sym) = toEncoding sym


instance ToJSON (Ref ()) where
    toJSON = undefined
    toEncoding (VarRef () var) = toEncoding var
    toEncoding (TagRef () tag) = toEncoding tag
    toEncoding (OpRef sym) = toEncoding sym


instance ToJSON AST.UnaryOperator where
    toJSON = undefined
    toEncoding Negative = toEncoding ("-" :: Text)

instance FromJSON AST.UnaryOperator where
    parseJSON = withText "UnaryOperator" $ \case
        "-" -> return AST.Negative
        other -> fail ("unexpected UnaryOperator (\"-\" is the only valid one): " <> show other)


instance ToJSON (AST.Listing AST.DetailedListing) where
    toJSON = undefined
    toEncoding = \case
        AST.ExplicitListing a multiline -> toEncoding a
        AST.OpenListing (C comments ()) -> toEncoding ("Everything" :: Text)
        AST.ClosedListing -> toEncoding Null

instance FromJSON (AST.Listing AST.DetailedListing) where
    parseJSON = \case
        Aeson.String "Everything" ->
            return $ AST.OpenListing (C ([], []) ())

        Aeson.Bool True ->
            return $ AST.OpenListing (C ([], []) ())

        Aeson.Null ->
            return AST.ClosedListing

        Aeson.Bool False ->
            return AST.ClosedListing

        json ->
            AST.ExplicitListing
                <$> parseJSON json
                <*> return False


instance ToJSON AST.DetailedListing where
    toJSON = undefined
    toEncoding = \case
        AST.DetailedListing values operators types ->
            pairs $ mconcat
                [ "values" .= Map.fromList (fmap (\(LowercaseIdentifier k) -> (k, True)) (Map.keys values))
                , "types" .= Map.fromList (fmap (\(UppercaseIdentifier k, C _ (C _ listing)) -> (k, listing)) (Map.toList types))
                ]

instance FromJSON AST.DetailedListing  where
    parseJSON = withObject "DetailedListing" $ \obj ->
        AST.DetailedListing
            <$> ((obj .:? "values" .!= Null) >>= parseValues)
            <*> return mempty
            <*> (fmap (C ([], []) . C []) <$> (obj .:? "types" .!= mempty))
        where
            parseValues = \case
                Aeson.Array json ->
                    Map.fromList . fmap (, C ([], []) ()) <$> parseJSON (Array json)

                Aeson.Null ->
                    return mempty

                json ->
                    -- TODO: ignore entries where value is False
                    fmap (C ([], []) . (\(b :: Bool) -> ())) <$> parseJSON json


instance ToJSON (AST.Listing (AST.CommentedMap UppercaseIdentifier ())) where
    toJSON = undefined
    toEncoding = \case
        AST.ExplicitListing tags _ ->
            toEncoding $ Map.fromList $ (\(UppercaseIdentifier k, C _ ()) -> (k, True)) <$> Map.toList tags
        AST.OpenListing (C _ ()) -> toEncoding ("AllTags" :: Text)
        AST.ClosedListing -> toEncoding ("NoTags" :: Text)

instance FromJSON (AST.Listing (AST.CommentedMap UppercaseIdentifier ())) where
    parseJSON = \case
        Aeson.String "AllTags" ->
            return $ AST.OpenListing (C ([], []) ())

        Aeson.Bool True ->
            return $ AST.OpenListing (C ([], []) ())

        Aeson.String "NoTags" ->
            return AST.ClosedListing

        Aeson.Bool False ->
            return AST.ClosedListing

        Aeson.Null ->
            return AST.ClosedListing

        json ->
            fail ("unexpected TagListing: " <> show json)

{-| An Int64 that encodes to a JSON String if necessary to preserve accuracy. -}
newtype SafeInt
    = SafeInt { fromSafeInt :: Int64 }

instance ToJSON SafeInt where
    toJSON = undefined
    toEncoding = \case
        SafeInt value ->
            if value <= 9007199254740991 && value >= -9007199254740991
                then toEncoding value
                else toEncoding $ show value

instance FromJSON SafeInt where
    parseJSON = \case
        Aeson.Number n -> SafeInt <$> parseJSON (Aeson.Number n)
        Aeson.String s -> SafeInt . read <$> parseJSON (Aeson.String s)
        _ -> fail "expected an integer (or a string representing an integer)"


instance ToJSON AST.LiteralValue where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs AST.LiteralValue where
    toPairs = \case
        IntNum value repr ->
            mconcat
                [ type_ "IntLiteral"
                , "value" .= SafeInt value
                , pair "display" $ pairs
                    ("representation" .= repr)
                ]

        FloatNum value repr ->
            mconcat
                [ type_ "FloatLiteral"
                , "value" .= value
                , pair "display" $ pairs
                    ("representation" .= repr)
                ]

        Boolean value ->
            mconcat
                [ type_ "ExternalReference"
                , "module" .= UppercaseIdentifier "Basics"
                , "identifier" .= show value
                ]

        Chr chr ->
            mconcat
                [ type_ "CharLiteral"
                , "value" .= chr
                ]

        Str str repr ->
            mconcat
                [ type_ "StringLiteral"
                , "value" .= str
                , pair "display" $ pairs
                    ("representation" .= repr)
                ]

instance FromJSON AST.LiteralValue  where
    parseJSON = withObject "LiteralValue" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "IntLiteral" ->
                AST.IntNum
                    <$> (fromSafeInt <$> obj .: "value")
                    <*> return DecimalInt

            "FloatLiteral" ->
                AST.FloatNum
                    <$> obj .: "value"
                    <*> return DecimalFloat

            "CharLiteral" ->
                AST.Chr
                    <$> obj .: "value"

            "StringLiteral" ->
                AST.Str
                    <$> obj .: "value"
                    <*> return SingleQuotedString

            _ ->
                fail ("unexpected LiteralValue tag: " <> tag)


instance FromJSON (Ref ()) where
    parseJSON = withText "Ref" $ \text ->
        case refFromText text of
            Nothing ->
                fail ("invalid Reference name: " <> Text.unpack text)

            Just ref ->
                return ref


instance ToJSON IntRepresentation where
    toEncoding = genericToEncoding defaultOptions


instance ToJSON FloatRepresentation where
    toEncoding = genericToEncoding defaultOptions


instance ToJSON StringRepresentation where
    toEncoding = genericToEncoding defaultOptions


instance (ToJSON a, ToJSON (f a)) => ToJSON (MaybeF f a) where
    toJSON = undefined
    toEncoding = \case
        JustF fa -> toEncoding fa
        NothingF a -> toEncoding a

instance (FromJSON (f a)) => FromJSON (MaybeF f a) where
    parseJSON json =
        -- TODO: should this fall back to parsing an `a`?
        JustF <$> parseJSON json



--
-- Stuff the should be removed later
--


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Reporting.Annotation.Located a
noRegion =
    Reporting.Annotation.at nowhere nowhere
