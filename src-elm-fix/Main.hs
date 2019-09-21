{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified ElmFix.Cli
import qualified System.Environment


main :: IO ()
main =
    do
        args <- System.Environment.getArgs
        ElmFix.Cli.main args
