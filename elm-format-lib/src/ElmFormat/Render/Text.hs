{-# LANGUAGE DataKinds #-}
module ElmFormat.Render.Text where

import Data.Coapplicative
import Elm.Utils ((|>))
import ElmVersion (ElmVersion)
import AST.Structure
import AST.V0_16

import AST.Module (Module)
import qualified Box
import qualified Data.Text as Text
import qualified ElmFormat.Render.Box as Render
import qualified Data.Fix as Fix
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified Data.Indexed as I


render :: Coapplicative annf => ElmVersion -> Module [UppercaseIdentifier] (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'TopLevelNK) -> Text.Text
render elmVersion modu =
    renderBox $ Fix.cata ElmStructure.render $ Render.formatModule elmVersion True 2 modu


renderBox :: Box.Box -> Text.Text
renderBox box =
    box
        |> Box.render


-- TODO: remove this and convert the Integration test to a test fixture
renderLiteral :: ElmVersion -> LiteralValue -> Text.Text
renderLiteral elmVersion literal =
    renderBox $ Fix.cata ElmStructure.render $ Render.formatLiteral elmVersion literal
