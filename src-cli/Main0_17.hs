{-# OPTIONS_GHC -Wall #-}
module Main where

import ElmVersion
import qualified ElmFormat


main :: IO ()
main =
    ElmFormat.main Elm_0_17
