{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Text where

import Elm.Utils ((|>))

import qualified AST.Module
import qualified Box
import qualified Data.Text.Lazy as LazyText
import qualified ElmFormat.Render.Box as Render


render :: AST.Module.Module -> LazyText.Text
render modu =
    let
        trimSpaces text =
            text
                |> LazyText.lines
                |> map LazyText.stripEnd
                |> LazyText.unlines
    in
        Render.formatModule modu
            |> Box.render . Box.depr
            |> LazyText.pack
            |> trimSpaces
