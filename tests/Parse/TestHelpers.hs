module Parse.TestHelpers where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

import Parse.Helpers (IParser, iParse)
import Reporting.Annotation hiding (map, at)
import Reporting.Region
import Text.ParserCombinators.Parsec.Combinator (eof)
import qualified Data.List as List
import qualified Data.List.Split as List


parseFullInput :: IParser a -> IParser a
parseFullInput parser =
    (\x _ -> x) <$> parser <*> eof


assertParse :: (Show a, Eq a) => IParser a -> String -> a -> Assertion
assertParse parser input expected =
    let
        output = iParse (parseFullInput parser) input
    in
        case output of
            Left err ->
                assertEqual (show err) False True
            Right result ->
                assertEqual input expected result


assertFailure :: (Show a, Eq a) => IParser a -> String -> Assertion
assertFailure parser input =
    let
        output = iParse (parseFullInput parser) input
    in
        case output of
            Left err ->
                assertEqual (show err) True True
            Right result ->
                assertEqual (show result) True False


nowhere = Region (Position 0 0) (Position 0 0)

at a b c d = A (Region (Position a b) (Position c d))


{-| Checks that removing indentation causes parsing to fail.

For each "\n " in the input string, a test case will be generated checking that
the given parser will fail if that "\n " is replaced by "\n".
-}
mustBeIndented parser input =
    input
        |> generateReplacements "\n " "\n"
        |> List.map (testCase "" . assertFailure parser)
        |> testGroup "must be indented"


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
