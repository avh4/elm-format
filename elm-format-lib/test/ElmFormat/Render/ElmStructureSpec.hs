module ElmFormat.Render.ElmStructureSpec where

import Elm.Utils ((|>))

import Test.Hspec
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text

import AST.V0_16
import Text.PrettyPrint.Avh4.Block as Block
import ElmFormat.Render.ElmStructure
import Data.Text (Text)
import qualified Data.Fix as Fix
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty


trim :: String -> String
trim text =
    text
        |> LazyText.pack
        |> LazyText.lines
        |> map LazyText.stripEnd
        |> LazyText.unlines
        |> LazyText.unpack


assertOutput :: String -> Elm -> Expectation
assertOutput expected actual =
    expected `shouldBe` trim (Text.unpack $ Block.render $ Fix.cata ElmStructure.render actual)


word :: Text -> Elm
word =
    ElmStructure.identifier


a :: Elm ; b :: Elm ; c :: Elm
[a,b,c] = word <$> ["a", "b", "c"]


block :: Text -> Elm
block text =
    ElmStructure.stack1 $ NonEmpty.fromList
        [ word (text<>text)
        , word (text<>text)
        ]


aa :: Elm ; bb :: Elm ; cc :: Elm
[aa,bb,cc] = block <$> ["a", "b", "c"]


spec :: Spec
spec = describe "ElmFormat.Render.ElmStructure" $ do
    it "application (single line)" $
        assertOutput "a b c\n" $
            application (FAJoinFirst JoinAll) a (b:|[c])
    it "application (multiline)" $
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

    it "group (empty)" $
        assertOutput "()\n" $
            group True "(" "," ")" False []
    it "group (single item, single line)" $
        assertOutput "( foo )\n" $
            group True "(" "," ")" False [ word "foo" ]
    it "group (single line)" $
        assertOutput "( foo, bar )\n" $
            group True "(" "," ")" False [ word "foo", word "bar" ]
    it "group (single line, no spaces)" $
        assertOutput "(foo, bar)\n" $
            group False "(" "," ")" False [ word "foo", word "bar" ]
    it "group (multiline)" $
        assertOutput "( aa\n  aa\n, b\n, cc\n  cc\n)\n" $
            group True "(" "," ")" False [ aa, b, cc ]
    it "group (forced multiline)" $
        assertOutput "( a\n, b\n, c\n)\n" $
            group True "(" "," ")" True [ a, b, c ]

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


shouldOutput :: Elm -> [Text] -> Expectation
shouldOutput elm expected =
    Block.render (Fix.cata ElmStructure.render elm)
    `shouldBe` Text.unlines expected
