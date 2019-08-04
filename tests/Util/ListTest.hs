module Util.ListTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Util.List


tests :: TestTree
tests = testGroup
  "Util.List"
  [ testGroup
    "pairs"
    [ testCase "" $ assertEqual "" [(1, 2), (2, 3)] $ pairs [1, 2, 3]
    , testCase "empty" $ assertEqual "" [] $ pairs ([] :: [Int])
    , testCase "single" $ assertEqual "" [] $ pairs [1]
    ]
  , testGroup
    "intersperseMap"
    [ testCase "" $ assertEqual "" [21, 20050, 51, 50070, 71] $ intersperseMap
      (\a b -> [1000 * a + b])
      (\a -> a + 1)
      [20, 50, 70]
    , testCase "empty" $ assertEqual "" [] $ intersperseMap (\a b -> [])
                                                            id
                                                            ([] :: [Int])
    ]
  , testGroup
    "shift"
    [ testCase "" $ assertEqual "" ([("a", 1), ("b", 2)], "c") $ shift
      "a"
      [(1, "b"), (2, "c")]
    , testCase "" $ assertEqual "empty" ([], "x") $ shift
      "x"
      ([] :: [(Int, String)])
    ]
  ]
