module Parse.TypeSpec where

import Test.Hspec hiding (example)

import qualified Parse.Type
import AST.V0_16
import AST.Structure

import Parse.TestHelpers
import ElmVersion
import ElmFormat.Render.Box (formatType, TypeParensRequired (NotRequired), typeParens)
import qualified Text.PrettyPrint.Avh4.Block as Block
import qualified Data.Text as Text
import Parse.IParser
import Reporting.Annotation (Located)
import qualified Data.Fix as Fix
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified Data.Indexed as I
import Data.Coapplicative (extract)


expr :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TypeNK)
expr = Parse.Type.expr


example :: String -> String -> String -> SpecWith ()
example name input expected =
    it name $
        assertParse (fmap (Text.unpack . Block.render . Fix.cata ElmStructure.render . typeParens NotRequired . formatType Elm_0_19 . I.fold2 (I.Fix . extract)) (expr Elm_0_19)) input expected


spec :: Spec
spec = describe "Parse.Type" $ do
    describe "tuple type" $ do
        example "" "(a,b)" "( a, b )\n"
        example "whitespace" "( a , b )" "( a, b )\n"
        example "comments"
            "({-A-}a{-B-},{-C-}b{-D-})"
            "( {- A -} a {- B -}, {- C -} b {- D -} )\n"
        example "newlines" "(\n a\n ,\n b\n )" "( a\n, b\n)\n"

    describe "record type" $ do
        describe "empty" $ do
            example "" "{}" "{}\n"
            example "whitespace" "{ }" "{}\n"
            example "comments" "{{-A-}}" "{{- A -}}\n"

        example ""
            "{x:m,y:n}"
            "{ x : m, y : n }\n"
        example "whitespace"
            "{ x : m , y : n }"
            "{ x : m, y : n }\n"
        example "comments"
            "{{-A-}x{-B-}:{-C-}m{-D-},{-E-}y{-F-}:{-G-}n{-H-}}"
            "{ {- A -} x {- B -} : {- C -} m\n\
            \\n\
            \{- D -}\n\
            \, {- E -} y {- F -} : {- G -} n\n\
            \\n\
            \{- H -}\n\
            \}\n"
        example "single field with comments"
            "{{-A-}x{-B-}:{-C-}m{-D-}}"
            "{ {- A -} x {- B -} : {- C -} m\n\
            \\n\
            \{- D -}\n\
            \}\n"
        example "newlines"
            "{\n x\n :\n m\n ,\n y\n :\n n\n }"
            "{ x :\n\
            \    m\n\
            \, y :\n\
            \    n\n\
            \}\n"

    describe "record extension type" $ do
        example ""
            "{a|x:m,y:n}"
            "{ a | x : m, y : n }\n"
        example "single field"
            "{a|x:m}"
            "{ a | x : m }\n"
        example "whitespace"
            "{ a | x : m , y : n }"
            "{ a | x : m, y : n }\n"
        example "comments"
            "{{-A-}a{-B-}|{-C-}x{-D-}:{-E-}m{-F-},{-G-}y{-H-}:{-I-}n{-J-}}"
            "{ {- A -} a {- B -}\n\
            \    | {- C -} x {- D -} : {- E -} m\n\
            \\n\
            \    {- F -}\n\
            \    , {- G -} y {- H -} : {- I -} n\n\
            \\n\
            \    {- J -}\n\
            \}\n"
        example "newlines"
            "{\n a\n |\n x\n :\n m\n ,\n y\n :\n n\n }"
            "{ a\n\
            \    | x :\n\
            \        m\n\
            \    , y :\n\
            \        n\n\
            \}\n"
        it "only allows simple base" $
            assertParseFailure (expr Elm_0_19) "{()|x:m}"
        it "only allows simple base" $
            assertParseFailure (expr Elm_0_19) "{{}|x:m}"
        example "no fields (elm-compiler does not allow this)"
            "{a|}"
            "{ a | }\n"
