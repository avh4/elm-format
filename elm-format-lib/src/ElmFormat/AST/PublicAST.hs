{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module ElmFormat.AST.PublicAST where

import ElmFormat.AST.Shared
import AST.V0_16 (NodeKind(..), AST)
import qualified AST.V0_16 as AST
import Data.Kind (Type)
import Reporting.Annotation (Located(A))
import Reporting.Region (Region)
import qualified Reporting.Region as Region
import Text.JSON hiding (showJSON)
import qualified Data.List as List
import AST.Structure (ASTNS, ASTNS1)
import Data.Indexed as I
import Data.Coapplicative


data ModuleName =
    ModuleName [UppercaseIdentifier]


data ExternalReference
    = ExternalReference
        { module_ :: ModuleName
        , identifier :: UppercaseIdentifier
        }


data Pattern
    = AnythingPattern
    | UnitPattern
    | LiteralPattern LiteralValue
    | VariableDefinition
        { name :: LowercaseIdentifier
        }
    | DataPattern
        { constructor :: ExternalReference
        , arguments :: List (Located Pattern)
        }
    | TODO_Pattern String



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
        VariableDefinition name

    AST.DataPattern (namespace, tag) args ->
        DataPattern
            (ExternalReference (ModuleName namespace) tag)
            (fmap (fromRawAST . extract) args)

    pat ->
        TODO_Pattern (show pat)



fromRawAST :: ASTNS Located [UppercaseIdentifier] 'PatternNK -> Located (Pattern)
fromRawAST =
    fmap fromRawAST' . I.unFix



--
-- JSON serialization
--


class ToJSON a where
  showJSON :: a -> JSValue


type_ :: String -> (String, JSValue)
type_ t =
    ("tag", JSString $ toJSString t)


sourceLocation :: Region -> (String, JSValue)
sourceLocation region =
    ( "sourceLocation", showJSON region )


instance ToJSON a => ToJSON [a] where
    showJSON = JSArray . fmap showJSON


instance ToJSON Region.Region where
    showJSON region =
        makeObj
            [ ( "start", showJSON $ Region.start region )
            , ( "end", showJSON $ Region.end region )
            ]


instance ToJSON Region.Position where
    showJSON pos =
        makeObj
            [ ( "line", JSRational False $ toRational $ Region.line pos )
            , ( "col", JSRational False $ toRational $ Region.column pos )
            ]


instance ToJSON UppercaseIdentifier where
    showJSON (UppercaseIdentifier name) = JSString $ toJSString name


instance ToJSON LowercaseIdentifier where
    showJSON (LowercaseIdentifier name) = JSString $ toJSString name


instance ToJSON IntRepresentation where
    showJSON DecimalInt = JSString $ toJSString "DecimalInt"
    showJSON HexadecimalInt = JSString $ toJSString "HexadecimalInt"


instance ToJSON FloatRepresentation where
    showJSON DecimalFloat = JSString $ toJSString "DecimalFloat"
    showJSON ExponentFloat = JSString $ toJSString "ExponentFloat"


instance ToJSON StringRepresentation where
    showJSON SingleQuotedString = JSString $ toJSString "SingleQuotedString"
    showJSON TripleQuotedString = JSString $ toJSString "TripleQuotedString"


instance ToJSON ModuleName where
    showJSON (ModuleName []) = JSNull
    showJSON (ModuleName namespace) = (JSString . toJSString . List.intercalate "." . fmap (\(UppercaseIdentifier v) -> v)) namespace


instance ToJSON (Located AST.LiteralValue) where
    showJSON (A region lit) =
        case lit of
            IntNum value repr ->
                makeObj
                    [ type_ "IntLiteral"
                    , ("value", JSRational False $ toRational value)
                    , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                    , sourceLocation region
                    ]

            FloatNum value repr ->
                makeObj
                    [ type_ "FloatLiteral"
                    , ("value", JSRational False $ toRational value)
                    , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                    , sourceLocation region
                    ]

            Boolean value ->
                makeObj
                    [ type_ "ExternalReference"
                    , ("module", JSString $ toJSString "Basics")
                    , ("identifier", JSString $ toJSString $ show value)
                    , sourceLocation region
                    ]

            Chr chr ->
                makeObj
                    [ type_ "CharLiteral"
                    , ("value", JSString $ toJSString [chr])
                    , sourceLocation region
                    ]

            Str str repr ->
                makeObj
                    [ type_ "StringLiteral"
                    , ("value", JSString $ toJSString str)
                    , ( "display"
                    , makeObj
                        [ ( "representation", showJSON repr )
                        ]
                    )
                    , sourceLocation region
                    ]


instance ToJSON (Located Pattern) where
    showJSON (A region pattern') =
        case pattern' of
            AnythingPattern ->
                makeObj
                    [ type_ "AnythingPattern"
                    , sourceLocation region
                    ]

            UnitPattern ->
                makeObj
                    [ type_ "UnitPattern"
                    , sourceLocation region
                    ]

            LiteralPattern lit ->
                showJSON (A region lit)

            VariableDefinition name ->
                makeObj
                    [ type_ "VariableDefinition"
                    , ( "name" , showJSON name )
                    , sourceLocation region
                    ]

            DataPattern constructor arguments ->
                makeObj
                    [ type_ "DataPattern"
                    , ( "constructor", showJSON constructor)
                    , ( "arguments", showJSON arguments)
                    , sourceLocation region
                    ]

            TODO_Pattern string ->
                JSString $ toJSString $ "TODO: Pattern (" ++ string ++ ")"


instance ToJSON ExternalReference where
    showJSON (ExternalReference module_ identifier) =
        makeObj
            [ type_ "ExternalReference"
            , ("module", showJSON module_)
            , ("identifier", showJSON identifier)
            ]
