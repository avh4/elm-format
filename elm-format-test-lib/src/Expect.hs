module Expect where

import Test.Tasty.HUnit ((@=?))


equals :: (Eq a, Show a) => a -> a -> IO ()
equals expected actual =
    expected @=? actual
