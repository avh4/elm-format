{-# LANGUAGE OverloadedStrings #-}
module Data.Text.ExtraSpec (spec) where

import Test.Hspec
import Data.Text.Extra


spec :: Spec
spec = describe "Data.Text.ExtraTest" $ do
    it "when there is no span of the given character" $
        longestSpanOf '*' "stars exist only where you believe"
            `shouldBe` NoSpan
    it "when the given character is present" $
        longestSpanOf '*' "it's here -> * <-"
            `shouldBe` Span 1
    it "only counts the longest span" $
        longestSpanOf '*' "it's here -> ** <-, not here: *"
            `shouldBe` Span 2
