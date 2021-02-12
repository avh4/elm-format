{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ElmFormat.AST.PublicAST where

import ElmFormat.AST.Shared
import AST.V0_16 (NodeKind(..), sequenceToList, Pair(..))
import qualified AST.V0_16 as AST
import Reporting.Annotation (Located(A))
import Reporting.Region (Region)
import qualified Reporting.Region as Region
import Text.JSON hiding (showJSON)
import qualified Data.List as List
import AST.Structure (ASTNS, ASTNS1)
import Data.Indexed as I
import Data.Maybe (listToMaybe)
import Data.Coapplicative
import Data.ReversedList (Reversed)
import qualified Data.ReversedList as ReversedList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data ModuleName =
    ModuleName [UppercaseIdentifier]


data ExternalReference
    = ExternalReference
        { module_ :: ModuleName
        , identifier :: UppercaseIdentifier
        }


data VariableDefinition
    = VariableDefinition
        { name :: LowercaseIdentifier
        }


data Pattern
    = AnythingPattern
    | UnitPattern
    | LiteralPattern LiteralValue
    | VariablePattern VariableDefinition
    | DataPattern
        { constructor :: ExternalReference
        , arguments :: List (Located Pattern) -- Non-empty
        }
    | TuplePattern
        { terms :: List (Located Pattern) -- At least two items
        }
    | ListPattern -- Construct with mkListPattern
        { prefix :: List (Located Pattern)
        , rest :: Maybe (Located Pattern) -- Must not be a ListPattern
        }
    | RecordPattern
        { fields :: List VariableDefinition
        }
    | PatternAlias
        { alias :: VariableDefinition
        , pattern :: Located Pattern
        }


mkListPattern :: List (Located Pattern) -> Maybe (Located Pattern) -> Pattern
mkListPattern prefix (Just (A _ (ListPattern prefix2 rest2))) =
    ListPattern (prefix ++ prefix2) rest2
mkListPattern prefix rest = ListPattern prefix rest


data Type_
    = UnitType
    | TypeReference
        { name_tr :: UppercaseIdentifier
        , module_ :: ModuleName
        , arguments :: List (Located Type_)
        }
    | TypeVariable
        { name_tv :: LowercaseIdentifier
        }
    | TupleType
        { terms :: List (Located Type_) -- At least two items
        }
    | RecordType
        { base :: Maybe LowercaseIdentifier
        , fields :: Map LowercaseIdentifier (Located Type_) -- Cannot be empty if base is present
        , display :: RecordTypeDisplay
        }
    | FunctionType
        { returnType :: Located Type_
        , argumentTypes :: List (Located Type_) -- Non-empty
        }


newtype RecordTypeDisplay
    = RecordTypeDisplay
        { fieldOrder :: List LowercaseIdentifier
        }


--
-- Transformation from raw AST to PublicAST
--


fromRawAST' :: ASTNS1 Located [UppercaseIdentifier] 'PatternNK -> Pattern
fromRawAST' = \case
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
            (ExternalReference (ModuleName namespace) tag)
            (fmap (fromRawAST . (\(C comments a) -> a)) args)

    AST.PatternParens (C (pre, post) pat) ->
        extract $ fromRawAST pat

    AST.TuplePattern terms ->
        TuplePattern
            (fmap (fromRawAST . (\(C comments a) -> a)) terms)

    AST.EmptyListPattern comments ->
        mkListPattern [] Nothing

    AST.ListPattern terms ->
        mkListPattern
            (fmap (fromRawAST . (\(C comments a) -> a)) terms)
            Nothing

    AST.ConsPattern (C firstEol first) rest ->
        let
            first' = fromRawAST first
            rest' = fmap (fromRawAST . (\(C comments a) -> a)) (sequenceToList rest)
        in
        case reverse rest' of
            [] -> mkListPattern [] (Just first')
            last : mid -> mkListPattern (first' : reverse mid) (Just last)

    AST.EmptyRecordPattern comment ->
        RecordPattern []

    AST.RecordPattern fields ->
        RecordPattern
            (fmap (VariableDefinition . (\(C comments a) -> a)) fields)

    AST.Alias (C comments1 pat) (C comments2 name) ->
        PatternAlias
            (VariableDefinition name)
            (fromRawAST pat)


typeFromRawAST' :: ASTNS1 Located [UppercaseIdentifier] 'TypeNK -> Type_
typeFromRawAST' = \case
    AST.UnitType comments ->
        UnitType

    AST.TypeConstruction (AST.NamedConstructor (namespace, name)) args forceMultine ->
        TypeReference
            name
            (ModuleName namespace)
            (fmap (\(C comments a) -> typeFromRawAST a) args)

    AST.TypeVariable name ->
        TypeVariable name

    AST.TypeParens (C comments t) ->
        typeFromRawAST' (extract $ I.unFix t)

    AST.TupleType terms multiline ->
        TupleType
            (fmap (\(C comments a) -> typeFromRawAST a) terms)

    AST.RecordType base fields comments multiline ->
        RecordType
            (fmap (\(C comments a) -> a) base)
            (Map.fromList $ fmap (\(C cp (Pair (C ck key) (C cv value) ml)) -> (key, typeFromRawAST value)) $ sequenceToList fields)
            $ RecordTypeDisplay
                (fmap (extract . _key . extract) $ sequenceToList fields)

    AST.FunctionType first rest multiline ->
        case firstRestToRestLast first (sequenceToList rest) of
            (args, C comments last) ->
                FunctionType
                    (typeFromRawAST last)
                    (fmap (\(C comments a) -> typeFromRawAST a) args)
    where
        firstRestToRestLast :: C0Eol x -> List (C2Eol a b x) -> (List (C2Eol a b x), C0Eol x)
        firstRestToRestLast first rest =
            done $ foldl (flip step) (ReversedList.empty, first) rest
            where
                step :: C2Eol a b x -> (Reversed (C2Eol a b x), C0Eol x) -> (Reversed (C2Eol a b x), C0Eol x)
                step (C (a, b, dn) next) (acc, C dn' last) =
                    (ReversedList.push (C (a, b, dn') last) acc, C dn next)

                done :: (Reversed (C2Eol a b x), C0Eol x) -> (List (C2Eol a b x), C0Eol x)
                done (acc, last) =
                    (ReversedList.toList acc, last)


fromRawAST :: ASTNS Located [UppercaseIdentifier] 'PatternNK -> Located Pattern
fromRawAST =
    fmap fromRawAST' . I.unFix


typeFromRawAST :: ASTNS Located [UppercaseIdentifier] 'TypeNK -> Located Type_
typeFromRawAST =
    fmap typeFromRawAST' . I.unFix


--
-- JSON serialization
--


data Config =
    Config
        { showSourceLocation :: Bool
        }


class ToJSON a where
  showJSON :: Config -> a -> JSValue


addField :: ( String, JSValue ) -> JSValue -> JSValue
addField field = \case
    JSObject obj ->
        JSObject
            $ toJSObject
            $ (++ [ field ])
            $ fromJSObject $ obj

    otherJson ->
        makeObj
            [ ( "value", otherJson )
            , field
            ]


type_ :: String -> (String, JSValue)
type_ t =
    ("tag", JSString $ toJSString t)


instance ToJSON a => ToJSON [a] where
    showJSON c = JSArray . fmap (showJSON c)


instance ToJSON a => ToJSON (Maybe a) where
    showJSON _ Nothing = JSNull
    showJSON c (Just a) = showJSON c a


instance ToJSON v => ToJSON (Map LowercaseIdentifier v) where
    showJSON c =
        makeObj
            . fmap (\((LowercaseIdentifier k), a) -> (k, showJSON c a))
            . Map.toList


instance ToJSON a => ToJSON (Located a) where
    showJSON c (A region a) =
        if showSourceLocation c
            then addField ( "sourceLocation", showJSON c region ) (showJSON c a)
            else showJSON c a


instance ToJSON Region where
    showJSON c region =
        makeObj
            [ ( "start", showJSON c $ Region.start region )
            , ( "end", showJSON c $ Region.end region )
            ]


instance ToJSON Region.Position where
    showJSON _ pos =
        makeObj
            [ ( "line", JSRational False $ toRational $ Region.line pos )
            , ( "col", JSRational False $ toRational $ Region.column pos )
            ]


instance ToJSON UppercaseIdentifier where
    showJSON _ (UppercaseIdentifier name) = JSString $ toJSString name


instance ToJSON LowercaseIdentifier where
    showJSON _ (LowercaseIdentifier name) = JSString $ toJSString name


instance ToJSON IntRepresentation where
    showJSON _ DecimalInt = JSString $ toJSString "DecimalInt"
    showJSON _ HexadecimalInt = JSString $ toJSString "HexadecimalInt"


instance ToJSON FloatRepresentation where
    showJSON _ DecimalFloat = JSString $ toJSString "DecimalFloat"
    showJSON _ ExponentFloat = JSString $ toJSString "ExponentFloat"


instance ToJSON StringRepresentation where
    showJSON _ SingleQuotedString = JSString $ toJSString "SingleQuotedString"
    showJSON _ TripleQuotedString = JSString $ toJSString "TripleQuotedString"


instance ToJSON ModuleName where
    showJSON _ (ModuleName []) = JSNull
    showJSON _ (ModuleName namespace) = (JSString . toJSString . List.intercalate "." . fmap (\(UppercaseIdentifier v) -> v)) namespace


instance ToJSON AST.LiteralValue where
    showJSON c = \case
        IntNum value repr ->
            makeObj
                [ type_ "IntLiteral"
                , ("value", JSRational False $ toRational value)
                , ("display"
                , makeObj
                    [ ("representation", showJSON c repr)
                    ]
                )
                ]

        FloatNum value repr ->
            makeObj
                [ type_ "FloatLiteral"
                , ("value", JSRational False $ toRational value)
                , ("display"
                , makeObj
                    [ ("representation", showJSON c repr)
                    ]
                )
                ]

        Boolean value ->
            makeObj
                [ type_ "ExternalReference"
                , ("module", JSString $ toJSString "Basics")
                , ("identifier", JSString $ toJSString $ show value)
                ]

        Chr chr ->
            makeObj
                [ type_ "CharLiteral"
                , ("value", JSString $ toJSString [chr])
                ]

        Str str repr ->
            makeObj
                [ type_ "StringLiteral"
                , ("value", JSString $ toJSString str)
                , ( "display"
                , makeObj
                    [ ( "representation", showJSON c repr )
                    ]
                )
                ]


instance ToJSON Pattern where
    showJSON c = \case
        AnythingPattern ->
            makeObj
                [ type_ "AnythingPattern"
                ]

        UnitPattern ->
            makeObj
                [ type_ "UnitPattern"
                ]

        LiteralPattern lit ->
            showJSON c lit

        VariablePattern def ->
            showJSON c def

        DataPattern constructor arguments ->
            makeObj
                [ type_ "DataPattern"
                , ( "constructor", showJSON c constructor )
                , ( "arguments", showJSON c arguments )
                ]

        TuplePattern terms ->
            makeObj
                [ type_ "TuplePattern"
                , ( "terms", showJSON c terms )
                ]

        ListPattern prefix rest ->
            makeObj
                [ type_ "ListPattern"
                , ( "prefix", showJSON c prefix )
                , ( "rest", showJSON c rest )
                ]

        RecordPattern fields ->
            makeObj
                [ type_ "RecordPattern"
                , ( "fields", showJSON c fields )
                ]

        PatternAlias alias pat ->
            makeObj
                [ type_ "PatternAlias"
                , ( "alias", showJSON c alias )
                , ( "pattern", showJSON c pat )
                ]


instance ToJSON Type_ where
    showJSON c = \case
        UnitType ->
            makeObj
                [ type_ "UnitType"
                ]

        TypeReference name module_ arguments ->
            makeObj
                [ type_ "TypeReference"
                , ( "name", showJSON c name )
                , ( "module", showJSON c module_ )
                , ( "arguments", showJSON c arguments )
                ]

        TypeVariable name ->
            makeObj
                [ type_ "TypeVariable"
                , ( "name", showJSON c name )
                ]

        TupleType terms ->
            makeObj
                [ type_ "TupleType"
                , ( "terms", showJSON c terms )
                ]

        RecordType Nothing fields display ->
            makeObj
                [ type_ "RecordType"
                , ( "fields", showJSON c fields )
                , ( "display", showJSON c display )
                ]

        RecordType (Just base) fields display ->
            makeObj
                [ type_ "RecordTypeExtension"
                , ( "base", showJSON c base )
                , ( "fields", showJSON c fields )
                , ( "display", showJSON c display )
                ]

        FunctionType returnType argumentTypes ->
            makeObj
                [ type_ "FunctionType"
                , ( "returnType", showJSON c returnType )
                , ( "argumentTypes", showJSON c argumentTypes )
                ]


instance ToJSON RecordTypeDisplay where
    showJSON c (RecordTypeDisplay fieldOrder) =
        makeObj
            [ ( "fieldOrder", showJSON c fieldOrder )
            ]


instance ToJSON ExternalReference where
    showJSON c (ExternalReference module_ identifier) =
        makeObj
            [ type_ "ExternalReference"
            , ("module", showJSON c module_)
            , ("identifier", showJSON c identifier)
            ]


instance ToJSON VariableDefinition where
    showJSON c (VariableDefinition name) =
        makeObj
            [ type_ "VariableDefinition"
            , ( "name" , showJSON c name )
            ]
