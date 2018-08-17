{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Text where

import Elm.Utils ((|>))
import ElmVersion (ElmVersion)
import AST.V0_16

import qualified AST.Module
import qualified Box
import qualified Data.Text as Text
import qualified ElmFormat.Render.Box as Render


render :: ElmVersion -> AST.Module.Module -> Text.Text
render elmVersion modu =
    renderBox $ Render.formatModule elmVersion modu


renderBox :: Box.Box -> Text.Text
renderBox box =
    box
        |> Box.render


-- TODO: remove this and convert the Integration test to a test fixture
renderLiteral :: ElmVersion -> Literal -> Text.Text
renderLiteral elmVersion literal =
    renderBox $ Render.formatLiteral elmVersion literal
