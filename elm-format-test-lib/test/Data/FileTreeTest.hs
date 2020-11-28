module Data.FileTreeTest where

import Elm.Utils ((|>))
import Data.FileTree (FileTree)
import qualified Data.FileTree as FileTree
import Data.Text (Text)
import Test.Tasty.Hspec
import System.FilePath (pathSeparator)


spec_spec :: Spec
spec_spec =
    describe "Data.FileTree" $ do
        it "can read and write a file" $ do
            (mempty :: FileTree Text)
                |> FileTree.write "/file.txt" "{}"
                |> FileTree.read "/file.txt"
                |> flip shouldBe (Just "{}")

        it "can read a non-existent file" $ do
            (mempty :: FileTree Text)
                |> FileTree.read "/file.txt"
                |> flip shouldBe Nothing

        it "normalizes paths" $ do
            (mempty :: FileTree Text)
                |> FileTree.write "/file.txt" "{}"
                |> FileTree.read (pathSeparator : "./file.txt")
                |> flip shouldBe (Just "{}")

        describe "doesFileExist" $ do
            it "is false for non-existant" $ do
                (mempty :: FileTree Text)
                    |> FileTree.doesFileExist "/file.txt"
                    |> flip shouldBe False

            it "is true for file" $ do
                (mempty :: FileTree Text)
                    |> FileTree.write "/file.txt" ""
                    |> FileTree.doesFileExist "/file.txt"
                    |> flip shouldBe True

            it "is false for directory" $ do
                (mempty :: FileTree Text)
                    |> FileTree.write "/path/file.txt" ""
                    |> FileTree.doesFileExist "/path"
                    |> flip shouldBe False

        describe "doesDirectoryExist" $ do
            it "is false for non-existant" $ do
                (mempty :: FileTree Text)
                    |> FileTree.doesDirectoryExist "/file.txt"
                    |> flip shouldBe False

            it "is false for file" $ do
                (mempty :: FileTree Text)
                    |> FileTree.write "/file.txt" ""
                    |> FileTree.doesDirectoryExist "/file.txt"
                    |> flip shouldBe False

            it "is true for directory" $ do
                (mempty :: FileTree Text)
                    |> FileTree.write "/path/file.txt" ""
                    |> FileTree.doesDirectoryExist "/path"
                    |> flip shouldBe True

        describe "listDirectory" $ do
            it "includes files" $ do
                (mempty :: FileTree Text)
                    |> FileTree.write "/path/file.txt" ""
                    |> FileTree.listDirectory "/path"
                    |> flip shouldBe ["file.txt"]

            it "includes directories" $ do
                (mempty :: FileTree Text)
                    |> FileTree.write "/path/sub/file.txt" ""
                    |> FileTree.listDirectory "/path"
                    |> flip shouldBe ["sub"]
