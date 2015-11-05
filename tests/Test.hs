module Main where

import Test.Framework

import Test.Property
import qualified BoxTest


main :: IO ()
main =
  defaultMain
    [ propertyTests
    , BoxTest.tests
    ]
