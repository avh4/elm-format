{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ElmFormat.AST.PublicAST where

import ElmFormat.AST.Shared
import AST.V0_16 (NodeKind(..), sequenceToList)
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
        , arguments :: List (Located Pattern)
        }
    | TuplePattern
        { terms :: List (Located Pattern)
        }
    | ListPattern -- Construct with mkListPattern
        { prefix :: List (Located Pattern)
        , rest :: Maybe (Located Pattern)
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


fromRawAST :: ASTNS Located [UppercaseIdentifier] 'PatternNK -> Located (Pattern)
fromRawAST =
    fmap fromRawAST' . I.unFix



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
