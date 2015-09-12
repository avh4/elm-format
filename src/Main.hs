{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText

main :: IO ()
main =
	"module Simple where\n\n\
	\token : String\n\
	\token =\n\
	\    \"XYZZY\"\n"
	|> LazyText.pack
	|> LazyText.writeFile "formatted.elm"
