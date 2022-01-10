module IndentTest where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck (Testable(property))
import qualified Indent


spec_spec :: Spec
spec_spec = describe "Indent" $
    let
        check a b expected =
            it (show a <> " <> " <> show b <> " == " <> show expected) $
                check' a b expected

        check' a b expected =
            Indent.spaces a <> Indent.spaces b
                `shouldBe` Indent.spaces expected
    in do
    it "full tabs should combine" $ property $
        \a b -> check' (4*a) (4*b)  (4*(a+b))

    it "zero is left identity" $ property $
        \x -> check' 0 x  x
    it "zero is right identity" $ property $
        \x -> check' x 0  x

    describe "when left is less than one tab" $ do
        check 1 4  4
        check 2 4  4
        check 3 4  4
        check 1 8  8

    it "when left is full tab, always add" $ property $
        \a b -> check' (4*a) b  (4*a + b)

    describe "when the sum does not exceed the next tabstop" $ do
        check 1 1  2
        check 1 2  3
        check 1 3  4

        check 2 1  3
        check 2 2  4

        check 6 1  7
        check 6 2  8
        check 5 3  8

    check 3 3  6  -- s s s s s s  => 6
    check 6 4  8  -- tab s s tab  => 8
    check 2 7  7  -- s s tab s s s  => 7
    check 6 7  11  -- tab s s tab s s s => 11

    it "associativity" $ property $
        \a b c ->
            (Indent.spaces a <> Indent.spaces b) <> Indent.spaces c
            `shouldBe`
            Indent.spaces a <> (Indent.spaces b <> Indent.spaces c)
