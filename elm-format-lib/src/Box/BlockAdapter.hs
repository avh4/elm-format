-- | This is a temporary module that mimics the https://hackage.haskell.org/package/prettyprint-avh4
-- API using the current `Box` module.
module Box.BlockAdapter where

import qualified Box
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy

type Line = Box.Line

space :: Line
space = Box.space

string7 :: String -> Line
string7 = Box.literal

char7 :: Char -> Line
char7 = Box.literal . pure

stringUtf8 :: String -> Line
stringUtf8 = Box.literal

lineFromBuilder :: B.Builder -> Line
lineFromBuilder = Box.literal . Lazy.unpack . Lazy.decodeUtf8 . B.toLazyByteString

commentByteString :: ByteString -> Line
commentByteString = Box.literal . Text.unpack . Text.decodeUtf8

type Block = Box.Box

render :: Block -> B.Builder
render = Text.encodeUtf8Builder . Box.render

blankLine :: Block
blankLine = Box.blankLine

line :: Line -> Block
line = Box.line

mustBreak :: Line -> Block
mustBreak = Box.mustBreak

stack :: NonEmpty Block -> Block
stack = Box.stack1 . NonEmpty.toList

stackForce :: Block -> Block -> Block
stackForce = Box.stack'

andThen :: [Block] -> Block -> Block
andThen = Box.andThen

indent :: Block -> Block
indent = Box.indent

prefix :: Word -> Line -> Block -> Block
prefix _ = Box.prefix

addSuffix :: Line -> Block -> Block
addSuffix = Box.addSuffix

joinMustBreak :: Block -> Block -> Block
joinMustBreak = Box.joinMustBreak

prefixOrIndent :: Maybe Line -> Line -> Block -> Block
prefixOrIndent = Box.prefixOrIndent

rowOrStack :: Maybe Line -> NonEmpty Block -> Block
rowOrStack = rowOrStackForce False

rowOrStackForce :: Bool -> Maybe Line -> NonEmpty Block -> Block
rowOrStackForce = Box.rowOrStackForce

rowOrIndent :: Maybe Line -> NonEmpty Block -> Block
rowOrIndent = rowOrIndentForce False

rowOrIndentForce :: Bool -> Maybe Line -> NonEmpty Block -> Block
rowOrIndentForce = Box.rowOrIndentForce
