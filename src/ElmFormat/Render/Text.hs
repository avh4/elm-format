{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Text where

import Elm.Utils ((|>))
import ElmVersion (ElmVersion)

import qualified AST.Module
import qualified Box
import qualified Data.Text as Text
import qualified ElmFormat.Render.Box as Render


render :: ElmVersion -> AST.Module.Module -> Text.Text
render elmVersion modu =
    let
        trimSpaces text =
            text
                |> Text.lines
                |> map Text.stripEnd
                |> Text.unlines
    in
        Render.formatModule elmVersion modu
            |> Box.render
            |> trimSpaces
