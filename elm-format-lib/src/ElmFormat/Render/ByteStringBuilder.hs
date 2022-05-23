module ElmFormat.Render.ByteStringBuilder (render, renderLiteral) where

import ElmVersion (ElmVersion)
import AST.Structure
import AST.V0_16

import qualified Text.PrettyPrint.Avh4.Block as Block
import qualified ElmFormat.Render.Box as Render
import qualified Data.Fix as Fix
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified Data.Indexed as I
import qualified Data.ByteString.Builder as B
import Text.PrettyPrint.Avh4.Block (Block)


render :: ElmVersion -> I.Fix (ASTNS [UppercaseIdentifier]) 'ModuleNK -> B.Builder
render elmVersion modu =
    renderBlock $ Fix.cata ElmStructure.render $ Render.formatModule elmVersion True 2 modu


renderBlock :: Block -> B.Builder
renderBlock = Block.render


-- TODO: remove this and convert the Integration test to a test fixture
renderLiteral :: ElmVersion -> LiteralValue -> B.Builder
renderLiteral elmVersion literal =
    renderBlock $ Fix.cata ElmStructure.render $ Render.formatLiteral elmVersion literal
