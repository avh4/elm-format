module Data.List.ExtraSpec where

import Test.Hspec

import Data.List.Extra


spec :: Spec
spec = describe "Util.List" $ do
    describe "pairs" $ do
        it "" $ pairs [1, 2, 3] `shouldBe` [(1,2), (2,3)]
        it "empty" $ pairs ([] :: [Int]) `shouldBe` []
        it "single" $ pairs [1] `shouldBe` []
    describe "intersperseMap" $ do
        it "" $
            intersperseMap (\a b -> [1000*a+b]) (\a -> a + 1) [ 20, 50, 70]
            `shouldBe` [ 21, 20050, 51, 50070, 71 ]
        it "empty" $
            intersperseMap (\a b -> []) id ([] :: [Int])
            `shouldBe` []
    describe "shift" $ do
        it "" $
            shift "a" [(1,"b"), (2,"c")]
            `shouldBe` ([("a",1), ("b",2)], "c")
        it "empty" $
            shift "x" ([] :: [(Int,String)])
            `shouldBe` ([],"x")
