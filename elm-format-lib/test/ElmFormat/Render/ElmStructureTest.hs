module ElmFormat.Render.ElmStructureTest where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text

import AST.V0_16
import Box
import ElmFormat.Render.ElmStructure
import Data.Text (Text)
import qualified Data.Fix as Fix
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import Test.Tasty.Hspec
import Data.List.NonEmpty(NonEmpty(..))


trim :: String -> String
trim text =
    text
        |> LazyText.pack
        |> LazyText.lines
        |> map LazyText.stripEnd
        |> LazyText.unlines
        |> LazyText.unpack


assertOutput :: String -> Elm -> Assertion
assertOutput expected actual =
    assertEqual expected expected $
        trim $ Text.unpack $ Box.render $ Fix.cata ElmStructure.render actual


word :: Text -> Elm
word =
    ElmStructure.identifier


a :: Elm ; b :: Elm ; c :: Elm
[a,b,c] = word <$> ["a", "b", "c"]


block :: Text -> Elm
block text =
    ElmStructure.stack1
        [ word (text<>text)
        , word (text<>text)
        ]


aa :: Elm ; bb :: Elm ; cc :: Elm
[aa,bb,cc] = block <$> ["a", "b", "c"]


test_tests :: TestTree
test_tests =
    testGroup "ElmFormat.Render.ElmStructure"
    [ testCase "application (single line)" $
        assertOutput "a b c\n" $
            application (FAJoinFirst JoinAll) a (b:|[c])
    , testCase "application (multiline)" $
        assertOutput
            ( unlines
                [ "aa"
                , "aa"
                , "    bb"
                , "    bb"
                , "    c"
                ]
            ) $
            application (FAJoinFirst JoinAll)
                aa
                ( bb :| [c] )

    , testCase "group (empty)" $
        assertOutput "()\n" $
            group True "(" "," ")" False []
    , testCase "group (single item, single line)" $
        assertOutput "( foo )\n" $
            group True "(" "," ")" False [ word "foo" ]
    , testCase "group (single line)" $
        assertOutput "( foo, bar )\n" $
            group True "(" "," ")" False [ word "foo", word "bar" ]
    , testCase "group (single line, no spaces)" $
        assertOutput "(foo, bar)\n" $
            group False "(" "," ")" False [ word "foo", word "bar" ]
    , testCase "group (multiline)" $
        assertOutput "( aa\n  aa\n, b\n, cc\n  cc\n)\n" $
            group True "(" "," ")" False [ aa, b, cc ]
    , testCase "group (forced multiline)" $
        assertOutput "( a\n, b\n, c\n)\n" $
            group True "(" "," ")" True [ a, b, c ]
    ]


spec_spec :: Spec
spec_spec =
    describe "ElmFormat.Render.ElmStructure" $ do

        describe "sectionedGroup" $ do
            describe "single item" $ do
                it "common" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" False
                        (a:|[])
                        [] Nothing
                    `shouldOutput`
                    [ "< a >" ]

                it "force multiline" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" True
                        (a:|[])
                        [] Nothing
                    `shouldOutput`
                    [ "< a"
                    , ">"
                    ]

                it "no inner spaces" $ do
                    ElmStructure.sectionedGroup
                        False "<" ";" ">" False
                        (a:|[])
                        [] Nothing
                    `shouldOutput`
                    [ "<a>" ]

                it "item is multiline" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" False
                        (aa:|[])
                        [] Nothing
                    `shouldOutput`
                    [ "< aa"
                    , "  aa"
                    , ">"
                    ]

            describe "multiple items" $ do
                it "common" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" False
                        (a:|[b, c])
                        [] Nothing
                    `shouldOutput`
                    [ "< a; b; c >" ]

                it "force multiline" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" True
                        (a:|[b, c])
                        [] Nothing
                    `shouldOutput`
                    [ "< a"
                    , "; b"
                    , "; c"
                    , ">"
                    ]

                it "has a multiline item" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" False
                        (aa:|[b, cc])
                        [] Nothing
                    `shouldOutput`
                    [ "< aa"
                    , "  aa"
                    , "; b"
                    , "; cc"
                    , "  cc"
                    , ">"
                    ]

                it "no inner spaces" $ do
                    ElmStructure.sectionedGroup
                        False "<" ";" ">" False
                        (a:|[b, c])
                        [] Nothing
                    `shouldOutput`
                    [ "<a; b; c>" ]

                it "multiline, no inner spaces" $ do
                    ElmStructure.sectionedGroup
                        False "<" ";" ">" False
                        (aa:|[b, cc])
                        [] Nothing
                    `shouldOutput`
                    [ "< aa"
                    , "  aa"
                    , "; b"
                    , "; cc"
                    , "  cc"
                    , ">"
                    ]

            describe "with additional sections" $ do
                it "common" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" False
                        (a:|[b])
                        [(word "label", b:|[c])] Nothing
                    `shouldOutput`
                    [ "< a"
                    , "; b"
                    , ""
                    , "label"
                    , "; b"
                    , "; c"
                    , ">"
                    ]

                it "multiline" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" False
                        (a:|[bb])
                        [] (Just $ word "extra")
                    `shouldOutput`
                    [ "< a"
                    , "; bb"
                    , "  bb"
                    , ""
                    , "extra"
                    , ">"
                    ]

            describe "with extra footer" $ do
                -- This fails because of https://github.com/avh4/elm-format/issues/760
                -- it "common" $ do
                --     ElmStructure.sectionedGroup
                --         True "<" ";" ">" True
                --         (a, [b])
                --         [] (Just $ word "extra")
                --     `shouldOutput`
                --     [ "< a; b extra >" ]

                it "force multiline" $ do
                    ElmStructure.sectionedGroup
                        True "<" ";" ">" True
                        (a:|[b])
                        [] (Just $ word "extra")
                    `shouldOutput`
                    [ "< a"
                    , "; b"
                    , ""
                    , "extra"
                    , ">"
                    ]

        describe "range" $ do
            it "common" $ do
                ElmStructure.range "[" ".." "]" a b
                `shouldOutput`
                [ "[a..b]" ]

            it "multiline" $ do
                ElmStructure.range "[" ".." "]" aa b
                `shouldOutput`
                [ "["
                , "    aa"
                , "    aa"
                , ".."
                , "    b"
                , "]"
                ]


shouldOutput :: Elm -> [Text] -> Expectation
shouldOutput elm expected =
    Box.render (Fix.cata ElmStructure.render elm)
    `shouldBe` Text.unlines expected
