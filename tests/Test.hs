module Main where

import Test.Framework
import Test.Property


main :: IO ()
main =
  defaultMain
    [ propertyTests
    ]
