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


render :: Coapplicative annf => ElmVersion -> Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] 'TopLevelNK) -> Text.Text
render elmVersion modu =
    renderBox $ Render.formatModule elmVersion True 2 modu


renderBox :: Box.Box -> Text.Text
renderBox box =
    box
        |> Box.render


-- TODO: remove this and convert the Integration test to a test fixture
renderLiteral :: ElmVersion -> LiteralValue -> Text.Text
renderLiteral elmVersion literal =
    renderBox $ Render.formatLiteral elmVersion literal
