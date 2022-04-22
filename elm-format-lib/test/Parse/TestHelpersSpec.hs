module Parse.TestHelpersSpec where

import Test.Hspec

import Parse.TestHelpers


spec :: Spec
spec = describe "TestHelpers" $ do
    describe "generateReplacements" $ do
        it "empty" $
            generateReplacements "a" "b" ""
            `shouldBe` []
        it "single match" $
            generateReplacements "a" "b" "1a2"
            `shouldBe` ["1b2"]
        it "multiple matches" $
            generateReplacements "a" "b" "a1a2a3a"
            `shouldBe`["b1a2a3a", "a1b2a3a", "a1a2b3a", "a1a2a3b"]
