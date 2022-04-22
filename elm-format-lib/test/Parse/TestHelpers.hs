module Parse.TestHelpers where

import Elm.Utils ((|>))

import Test.Hspec

import AST.V0_16
import AST.Structure
import Data.Indexed as I
import Parse.ParsecAdapter (eof)
import Parse.Helpers (iParse)
import Parse.IParser
import Reporting.Annotation (Located, Region)
import qualified Reporting.Annotation as A
import qualified Data.List as List
import qualified Data.List.Split as List
import Data.Word (Word16)


parseFullInput :: IParser a -> IParser a
parseFullInput parser =
    (\x _ -> x) <$> parser <*> eof


assertParse :: (Show a, Eq a) => IParser a -> String -> a -> Expectation
assertParse parser input expected =
    let
        output = iParse (parseFullInput parser) input
    in
        case output of
            Left err ->
                expectationFailure (show err)
            Right result ->
                expected `shouldBe` result


assertParseFailure :: (Show a) => IParser a -> String -> Expectation
assertParseFailure parser input =
    let
        output = iParse (parseFullInput parser) input
    in
        case output of
            Left _ ->
                pure ()
            Right result ->
                expectationFailure ("Expected parse failure, but parsed: " <> show result)


nowhere :: Region
nowhere = A.Region (A.Position 0 0) (A.Position 0 0)

at ::
    Word16 -> Word16 -> Word16 -> Word16
   -> AST (VariableNamespace  ns) (I.Fix2 Located (ASTNS ns)) kind
   -> I.Fix2 Located (ASTNS ns) kind
at a b c d = I.Fix2 . A.At (A.Region (A.Position a b) (A.Position c d))


{-| Checks that removing indentation causes parsing to fail.

For each "\n " in the input string, a test case will be generated checking that
the given parser will fail if that "\n " is replaced by "\n".
-}
mustBeIndented :: Show a => IParser a -> [Char] -> SpecWith ()
mustBeIndented parser input =
    describe "must be indented" $ do
        input
        |> generateReplacements "\n " "\n"
        |> List.map (it "" . assertParseFailure parser)
        |> sequence_


generateReplacements :: (Eq a) => [a] -> [a] -> [a] -> [[a]]
generateReplacements needle replacement input =
    input
        |> List.splitOn needle
        |> step'
    where
        step' rights =
            case rights of
                [] ->
                    []
                (next:rest) ->
                    step [] [next] rest

        step acc lefts rights =
            case rights of
                [] ->
                    acc
                (next:rest) ->
                    [lefts, rights]
                        |> List.map (List.intercalate needle)
                        |> List.intercalate replacement
                        |> flip (:) (step acc (lefts++[next]) rest)
