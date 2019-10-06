module Main where

import qualified ElmRefactor.Cli as Cli
import qualified System.Environment


main :: IO ()
main =
    do
        args <- System.Environment.getArgs
        Cli.main args
