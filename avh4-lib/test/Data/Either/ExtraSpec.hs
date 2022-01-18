module Data.Either.ExtraSpec where

import Data.Either.Extra
import Test.Hspec


spec :: Spec
spec =
    describe "Data.Either.Extra" $ do

        describe "delimit" $ do

            it "groups the inputs" $ do
                delimit
                    ([ Right 1
                    , Left "A"
                    , Right 2, Right 3
                    , Left "B"
                    ] :: [Either String Int])
                `shouldBe`
                ( [ 1 ]
                , [ ( "A", [2, 3])
                  , ( "B", [] )
                  ]
                )
