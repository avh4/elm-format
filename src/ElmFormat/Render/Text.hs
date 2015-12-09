{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Text where

import Elm.Utils ((|>))

import qualified AST.Module
import qualified Box
import qualified Data.Text as Text
import qualified ElmFormat.Render.Box as Render


render :: AST.Module.Module -> Text.Text
render modu =
    let
        trimSpaces text =
            text
                |> Text.lines
                |> map Text.stripEnd
                |> Text.unlines
    in
        Render.formatModule modu
            |> Box.render
            |> trimSpaces
