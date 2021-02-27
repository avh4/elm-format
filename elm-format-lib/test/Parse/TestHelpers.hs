module Parse.TestHelpers where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit

import AST.V0_16
import AST.Structure
import Data.Indexed as I
import Parse.Helpers (iParse)
import Parse.IParser
import Reporting.Annotation hiding (at)
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


assertParseFailure :: (Show a) => IParser a -> String -> Assertion
assertParseFailure parser input =
    let
        output = iParse (parseFullInput parser) input
    in
        case output of
            Left err ->
                assertEqual (show err) True True
            Right result ->
                assertEqual (show result) True False


nowhere :: Region
nowhere = Region (Position 0 0) (Position 0 0)

at ::
    Int -> Int -> Int -> Int
   -> AST (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns) (ASTNS Located ns) kind
   -> ASTNS Located ns kind
at a b c d = I.Fix . A (Region (Position a b) (Position c d))


{-| Checks that removing indentation causes parsing to fail.

For each "\n " in the input string, a test case will be generated checking that
the given parser will fail if that "\n " is replaced by "\n".
-}
mustBeIndented :: Show a => IParser a -> [Char] -> TestTree
mustBeIndented parser input =
    input
        |> generateReplacements "\n " "\n"
        |> List.map (testCase "" . assertParseFailure parser)
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
