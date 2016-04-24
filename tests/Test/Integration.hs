module Test.Integration (tests) where

import Elm.Utils ((|>))

import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Test.HUnit (Assertion, assertEqual, assertBool, assertFailure)
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Turtle
import qualified Filesystem.Path.CurrentOS as Path
import qualified Control.Foldl as Fold
import qualified Data.List as List


elmFormatPath = pack "./dist/build/elm-format/elm-format"


compareFiles :: String -> String -> Assertion
compareFiles expected actual =
  do
    (exitStatus, output) <-
      Turtle.procStrict (pack "diff")
        [ pack "-u"
        , pack actual
        , pack expected
        ]
        Turtle.empty
    case exitStatus of
      Turtle.ExitSuccess ->
        return ()

      _ ->
        assertFailure $ unpack output


checkGood :: String -> Test
checkGood filename =
  testCase filename $
    do
      let input = "tests/test-files/good/" ++ filename
      let output = "formatted.elm"
      exitStatus <- Turtle.proc elmFormatPath
        [ pack input
        , pack "--output"
        , pack output
        ]
        Turtle.empty
      case exitStatus of
        Turtle.ExitSuccess ->
          compareFiles input output
        _ ->
          assertFailure "elm-format produced an error"


checkGoodAllSyntax :: String -> Test
checkGoodAllSyntax name =
  testGroup ("AllGood: " ++ name)
    [ checkGood ("AllSyntax/" ++ name ++ ".elm")
    , checkGood ("AllSyntax/BlockComments/" ++ name ++ ".elm")
    , checkGood ("AllSyntax/LineComments/" ++ name ++ ".elm")
    ]


getStderr :: Fold.Fold (Either Turtle.Text Turtle.Text) Turtle.Text
getStderr =
  let
    step b a =
      case a of
        Right stdout ->
          b
        Left stderr ->
          stderr : b
  in
    Fold.Fold step [] (Text.unlines . List.reverse)


checkBad :: String -> Test
checkBad name =
  testCase ("(file with errors) " ++ name) $
    do
      let input = Path.fromText $ pack $ "tests/test-files/bad/" ++ name ++ ".elm"
      expected <- Turtle.strict $ Turtle.input $ Path.fromText $ pack $ "tests/test-files/bad/" ++ name ++ ".stderr.txt"

      () <- Turtle.cp input (Path.fromText $ pack $ "_input.elm")

      output <-
        (flip Turtle.fold) getStderr $
        Turtle.inprocWithErr elmFormatPath
          [ pack "--yes"
          , pack "_input.elm"
          ]
          Turtle.empty

      assertEqual "" expected $ output




      -- //
      -- // function checkTransformation() {
      -- // 	INPUT="tests/test-files/transform/$1"
      -- // 	OUTPUT="formatted.elm"
      -- // 	EXPECTED="tests/test-files/transform/${1%.*}.formatted.elm"
      -- //
      -- // 	time "$ELM_FORMAT" "$INPUT" --output "$OUTPUT" 1>/dev/null
      -- // 	returnCodeShouldEqual 0
      -- // 	compareFiles "$EXPECTED" "$OUTPUT"
      -- // }


checkTransformation :: String -> Test
checkTransformation name =
  testCase name $
    do
      let input = "tests/test-files/transform/" ++ name ++ ".elm"
      let output = "formatted.elm"
      let expected = "tests/test-files/transform/" ++ name ++ ".formatted.elm"

      exitStatus <- Turtle.proc elmFormatPath
        [ pack input
        , pack "--output"
        , pack output
        ]
        Turtle.empty

      case exitStatus of
        Turtle.ExitSuccess ->
          compareFiles expected output
        _ ->
          assertFailure "elm-format produced an error"


tests :: Test
tests =
  testGroup "Integration tests"
    [ testGroup "ways to run"
      [ testCase "help text" $
          let
            getHelpText args =
              (flip Turtle.fold) getStderr $
              Turtle.inprocWithErr elmFormatPath
                (map pack args)
                Turtle.empty
          in
            do
              help <- getHelpText ["--help"]
              shortHelp <- getHelpText ["-h"]
              noargs <- getHelpText []
              case (help == shortHelp, help == noargs) of
                (True, True) ->
                  return ()
                _ ->
                  assertFailure "help test doesn't match"
      ]

    , testGroup "properly-formatted input files"
      [ checkGood "Simple.elm"
      , checkGood "AllSyntax/AllSyntax.elm"
      , checkGoodAllSyntax "Module"
      , checkGoodAllSyntax "Declarations"
      , checkGoodAllSyntax "Patterns"
      , checkGoodAllSyntax "Types"
      , checkGoodAllSyntax "Expressions"
      , checkGood "Comments.elm"
      , checkGood "AllSyntax/GLShader.elm"
      , checkGood "AllSyntax/Literals.elm"
      , checkGood "AllSyntax/Comments.elm"
      , checkGood "ApiSketch.elm"
      ]

    , testGroup "properly-formatted real-world examples"
      [ checkGood "evancz/start-app/StartApp.elm"
      , checkGood "TheSeamau5/elm-check/Check.elm"
      , checkGood "rtfeldman/dreamwriter/Editor.elm"
      , checkGood "rtfeldman/dreamwriter/LeftSidebar.elm"
      , checkGood "rtfeldman/dreamwriter/RightSidebar.elm"
      , checkGood "rtfeldman/dreamwriter/WordGraph.elm"
      , checkGood "avh4/elm-fifo/Fifo.elm"
      ]

    , testGroup "files with syntax errors"
      [ checkBad "Empty"
      , checkBad "UnexpectedComma"
      , checkBad "UnexpectedEndOfInput"
      ]

    , testGroup "transformations"
      [ checkTransformation "Examples"
      , checkTransformation "QuickCheck-4562ebccb71ea9f622fb99cdf32b2923f6f9d34f-2529668492575674138"
      ]
    ]
