{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import qualified Flags
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText

main :: IO ()
main =
    do  config <- Flags.parse

        LazyText.readFile (Flags._file config)
        >>= LazyText.writeFile "formatted.elm"
