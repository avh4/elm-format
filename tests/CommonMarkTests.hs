module CommonMarkTests (construct) where

import qualified CMark
import Prelude hiding (init)
import Test.Hspec hiding (example)
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO
import qualified ElmFormat.Render.Markdown
import qualified Parse.Markdown


data State = CollectingInput | CollectingOutput | None
    deriving Eq

data ParseState = ParseState
    { path :: String
    , input :: [String]
    , output :: [String]
    , state :: State
    , siblings :: [Spec]
    , children :: [Spec]
    , example :: Int
    }


init :: ParseState
init = ParseState
    { path = ""
    , input = []
    , output = []
    , state = None
    , siblings = []
    , children = []
    , example = 1
    }


step :: ParseState -> Text.Text -> ParseState
step (ParseState path input output state siblings children example) line =
    if state == None && fmap fst (Text.uncons line) == Just '#' then
        -- validate input, output is []
        ParseState
            { path = Text.unpack line
            , input = []
            , output = []
            , state = None
            , siblings =
                if null children
                    then siblings
                    else describe path (sequence_ $ reverse children) : siblings
            , children = []
            , example = example
            }
    else if Text.unpack line == "```````````````````````````````` example" then
        -- validate input, output is [], state is None
        ParseState
            { path = path
            , input = []
            , output = []
            , state = CollectingInput
            , siblings = siblings
            , children = children
            , example = example
            }
    else if Text.unpack line == "````````````````````````````````" then
        -- validate state == CollectingOutput
        ParseState
            { path = path
            , input = []
            , output = []
            , state = None
            , siblings = siblings
            , children = makeTest example (concat $ reverse input) (unlines $ reverse input) (unlines $ reverse output) : children
            , example = example + 1
            }
    else if state == CollectingInput && Text.unpack line == "." then
        -- validate output == []
        ParseState
            { path = path
            , input = input
            , output = []
            , state = CollectingOutput
            , siblings = siblings
            , children = children
            , example = example
            }

    else
        case state of
            None ->
                ParseState path input output state siblings children example

            CollectingInput ->
                ParseState
                    { path = path
                    , input = Text.unpack line : input
                    , output = []
                    , state = state
                    , siblings = siblings
                    , children = children
                    , example = example
                    }

            CollectingOutput ->
                ParseState
                    { path = path
                    , input = input
                    , output = Text.unpack line : output
                    , state = state
                    , siblings = siblings
                    , children = children
                    , example = example
                    }


done :: ParseState -> [Spec]
done (ParseState path input output state siblings children _) =
    -- validate parse finished cleanly?
    reverse $ describe path (sequence_ children) : siblings


makeTest :: Int -> String -> String -> String -> Spec
makeTest i name input output =
  let
      source = Strict.map (\c -> if c == '→' then '\t' else c) $ Strict.pack $ input

      formatted = ElmFormat.Render.Markdown.formatMarkdown (const Nothing) (Parse.Markdown.parse $ Strict.unpack source)

      -- specOutput = Strict.map (\c -> if c == '→' then '\t' else c) $ Strict.pack output
  in
  it ("Example " ++ show i ++ ": " ++ name) $
        CMark.commonmarkToHtml [] (Strict.pack formatted)
        `shouldBe` CMark.commonmarkToHtml [] source


construct :: IO Spec
construct =
    do
        spec <- TextIO.readFile "tests/test-files/CommonMark/spec.txt"
        return $
            describe "CommonMark" $
                sequence_ $ done $ foldl step init (Text.lines spec)
