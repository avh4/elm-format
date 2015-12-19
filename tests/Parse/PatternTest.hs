module Parse.PatternTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Text.Lazy as LazyText

import Parse.Pattern
import Parse.Helpers (IParser, iParse)
import AST.V0_15
import AST.Pattern
import AST.Literal
import AST.Variable
import Reporting.Annotation hiding (map, at)
import Reporting.Region
import Text.Parsec.Char (string)

import Parse.TestHelpers


tests :: Test
tests =
    testGroup "Parse.Pattern"
    [ testCase "anything" $
        assertParse term "_" $ at 1 1 1 2 Anything
    ]
