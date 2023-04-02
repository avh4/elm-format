module ElmFormat.Render.BoxTest where

import AST.V0_16
import Box.BlockAdapter (Block)
import qualified Box.BlockAdapter as Block
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Builder as B
import Data.Functor.Identity (Identity (..))
import Data.Indexed (Fix (..))
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import ElmFormat.ImportInfo (ImportInfo (..))
import qualified ElmFormat.Render.Box as Render
import qualified ElmVersion
import Test.Hspec

spec_spec :: Spec
spec_spec =
  describe "ElmFormat.Render.Box" $ do
    --
    -- Declarations
    --

    it "eol comments after type annotation's colon start on a new line" $
      Render.formatCommonDeclaration
        ElmVersion.Elm_0_19
        (ImportInfo mempty Bimap.empty mempty mempty mempty)
        ( Fix $
            Identity $
              TypeAnnotation
                (C [] (VarRef () (LowercaseIdentifier "a")))
                (C [LineComment " Comment"] (Fix $ Identity $ UnitType []))
        )
        `shouldFormatAs` [ "a :",
                           "    -- Comment",
                           "    ()"
                         ]

    --
    -- Expression
    --

    it "eol comment inside unit keeps multiline" $
      Render.syntaxParens
        Render.SyntaxSeparated
        ( Render.formatExpression
            ElmVersion.Elm_0_19
            (ImportInfo mempty Bimap.empty mempty mempty mempty)
            (Fix $ Identity $ Unit [LineComment "A"])
        )
        `shouldFormatAs` [ "(--A",
                           ")"
                         ]

shouldFormatAs :: Block -> [Lazy.Text] -> Expectation
shouldFormatAs block expected =
  Lazy.decodeUtf8 (B.toLazyByteString (Block.render block))
    `shouldBe` Lazy.unlines expected
