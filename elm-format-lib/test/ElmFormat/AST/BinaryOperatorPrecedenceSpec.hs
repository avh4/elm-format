{-# OPTIONS_GHC -Wno-type-defaults #-}
module ElmFormat.AST.BinaryOperatorPrecedenceSpec where

import Prelude hiding (or)
import Test.Hspec
import qualified ElmFormat.AST.BinaryOperatorPrecedence as BinaryOperatorPrecedence
import ElmFormat.AST.BinaryOperatorPrecedence (Tree(..), Precedence(..), Associativity(..))
import ElmFormat.AST.Shared
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


spec :: Spec
spec =
    describe "ElmFormat.AST.BinaryOperatorPrecedence" $ do
        it "trivial case: no operators" $ do
            parse "A" []
            `shouldOk`
                Leaf "A"

        it "trivial case: single operator" $ do
            parse "A" [ ( "<|", "B" ) ]
            `shouldOk`
                Branch "<|" (Leaf "A") (Leaf "B")

        it "right associates (1)" $ do
            parse "A" [ ( "<|", "B" ), ( "<|", "C" ) ]
            `shouldOk`
                Branch "<|"
                    (Leaf "A")
                    (Branch "<|" (Leaf "B") (Leaf "C"))

        it "right associates (2)" $ do
            parse "A" [ ( "<|", "B" ), ( "<|", "C" ), ( "<|", "D" ) ]
            `shouldOk`
                Branch "<|"
                    (Leaf "A")
                    (Branch "<|"
                        (Leaf "B")
                        (Branch "<|" (Leaf "C") (Leaf "D")))

        it "left associates (1)" $ do
            parse "A" [ ( "|>", "B" ), ( "|>", "C" ) ]
            `shouldOk`
                Branch "|>"
                    (Branch "|>" (Leaf "A") (Leaf "B"))
                    (Leaf "C")

        it "left associates (2)" $ do
            parse "A" [ ( "|>", "B" ), ( "|>", "C" ), ( "|>", "D" ) ]
            `shouldOk`
                Branch "|>"
                    (Branch "|>"
                        (Branch "|>" (Leaf "A") (Leaf "B"))
                        (Leaf "C"))
                    (Leaf "D")

        it "precedence overrides right associativity" $ do
            parse "A" [ ( "||", "B" ), ( "<|", "C" ) ]
            `shouldOk`
                Branch "<|"
                    (Branch "||" (Leaf "A") (Leaf "B"))
                    (Leaf "C")

        it "precedence overrides left associativity" $ do
            parse "A" [ ( "|>", "B" ), ( "+", "C" ) ]
            `shouldOk`
                Branch "|>"
                    (Leaf "A")
                    (Branch "+" (Leaf "B") (Leaf "C"))

        it "allows a single non-associate operator" $ do
            parse "A" [ ( "==", "B" ) ]
            `shouldOk`
                Branch "=="
                    (Leaf "A")
                    (Leaf "B")

        it "errors with non-associative operators" $ do
            parse "A" [ ( "==", "B" ), ( "==", "C" ) ]
            `shouldError`
                ("conflicting associativity" :: Text)

        it "allows non-associative operators with difference precedence (1)" $ do
            parse "A" [ ( "==", "B" ), ( "=:=", "C" ) ]
            `shouldOk`
                Branch "=="
                    (Leaf "A")
                    (Branch "=:=" (Leaf "B") (Leaf "C"))

        it "allows non-associative operators with difference precedence (2)" $ do
            parse "A" [ ( "=:=", "B" ), ( "==", "C" ) ]
            `shouldOk`
                Branch "=="
                    (Branch "=:=" (Leaf "A") (Leaf "B"))
                    (Leaf "C")

        it "errors with conflicting associativity" $ do
            parse "A" [ ( "<|", "B" ), ( "|>", "C" ) ]
            `shouldError`
                ("conflicting associativity" :: Text)


parse :: e -> List (Text, e) -> Either Text (Tree Text e)
parse = BinaryOperatorPrecedence.parsePrecedence testOperators


shouldOk :: (HasCallStack, Show x, Show a, Eq x, Eq a) => Either x a -> a -> Expectation
shouldOk actual expected =
    actual
        `shouldBe` Right expected

shouldError :: (HasCallStack, Show x, Show a, Eq x, Eq a) => Either x a -> x -> Expectation
shouldError actual expected =
    actual
        `shouldBe` Left expected


testOperators :: Map Text Precedence
testOperators =
    Map.fromList $ fmap (\(op, p, a) -> (op, Precedence p a))
        -- From Elm 0.19
        [ ( "<|", 0, RightAssociate )
        , ( "|>", 0, LeftAssociate )
        , ( "||", 2, RightAssociate )
        , ( "==", 4, NonAssociate )
        , ( "+", 6, LeftAssociate )

        -- For testing
        , ( "=:=", 5, NonAssociate )
        ]
