module Integration.LiteralSpec (spec) where

import Elm.Utils ((|>))
import Test.Hspec

import qualified Data.ByteString.Builder as B
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.ByteStringBuilder as Render
import qualified ElmVersion
import qualified Reporting.Error.Syntax


spec :: Spec
spec = describe "Literals" $
    mapM_ makeTest
    [ -- Booleans
    --   ("True", "True\n")
    -- , ("False", "False\n")

    -- Integers
      ("1", "1\n")
    , ("0", "0\n")
    , ("-1", "-1\n")
    , ("536870911", "536870911\n")
    , ("-536870911", "-536870911\n")
    , ("0x00", "0x00\n")
    , ("0xFF", "0xFF\n")
    , ("0x07FF", "0x07FF\n")
    , ("0x00010000", "0x00010000\n")
    , ("0x0000000100000000", "0x0000000100000000\n")
    -- capitalize hex digits
    , ("0xabcdef00", "0xABCDEF00\n")
    -- format as full byte, word, double word
    , ("0x1", "0x01\n")
    , ("0x111", "0x0111\n")
    , ("0x11111", "0x00011111\n")
    , ("0x111111111", "0x0000000111111111\n")

    -- Floats
    , ("2.0", "2.0\n")
    , ("0.01", "0.01\n")
    -- e-notation
    , ("1.0e-2", "1.0e-2\n")
    , ("9.11e23", "9.11e23\n")
    ]


makeTest :: (Text, Text) -> SpecWith (Arg Expectation)
makeTest (original, formatted) =
    it (Text.unpack original) $
        Right (Lazy.fromStrict formatted)
        `shouldBe` format original


format :: Text -> Either [Reporting.Error.Syntax.Error] Lazy.Text
format source =
    Lazy.decodeUtf8 . B.toLazyByteString . Render.renderLiteral ElmVersion.Elm_0_18 <$> Parse.toEither (Parse.parseLiteral (Text.encodeUtf8 source))
