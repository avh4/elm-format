module Main where

import qualified ElmFormat.Cli
import qualified System.Environment


main :: IO ()
main =
    do
        args <- System.Environment.getArgs
        ElmFormat.Cli.main args

