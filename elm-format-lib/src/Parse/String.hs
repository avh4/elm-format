-- This module is based on `Parse.String` in the Elm compiler
-- https://github.com/elm/compiler/blob/94715a520f499591ac6901c8c822bc87cd1af24f/compiler/src/Parse/String.hs

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parse.String
  ( string
  , character
  )
  where


import qualified Data.Utf8 as Utf8
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)

import qualified Elm.String as ES
import Parse.Primitives (Parser, Row, Col)
import qualified Parse.Number as Number
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E

import AST.V0_16 ( StringRepresentation(SingleQuotedString, TripleQuotedString) )



-- CHARACTER


character :: (Row -> Col -> x) -> (E.Char -> Row -> Col -> x) -> Parser x ES.String
character toExpectation toError =
  P.Parser $ \(P.State src pos end indent row col nl sn) cok _ cerr eerr ->
    if pos >= end || P.unsafeIndex pos /= 0x27 {- ' -} then
      eerr row col toExpectation

    else
      case chompChar (plusPtr pos 1) end row (col + 1) 0 placeholder of
        Good newPos newCol numChars mostRecent ->
          if numChars /= 1 then
            cerr row col (toError (E.CharNotString (fromIntegral (newCol - col))))
          else
            let
              !newState = P.State src newPos end indent row newCol nl sn
              !char = ES.fromChunks [mostRecent]
            in
            cok char newState

        CharEndless newCol ->
          cerr row newCol (toError E.CharEndless)

        CharEscape r c escape ->
          cerr r c (toError (E.CharEscape escape))


data CharResult
  = Good (Ptr Word8) Col Word16 ES.Chunk
  | CharEndless Col
  | CharEscape Row Col E.Escape


chompChar :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Word16 -> ES.Chunk -> CharResult
chompChar pos end row col numChars mostRecent =
  if pos >= end then
    CharEndless col

  else
    let
      !word = P.unsafeIndex pos
    in
      if word == 0x27 {- ' -} then
        Good (plusPtr pos 1) (col + 1) numChars mostRecent

      else if word == 0x0A {- \n -} then
        CharEndless col

      else if word == 0x22 {- " -} then
        chompChar (plusPtr pos 1) end row (col + 1) (numChars + 1) doubleQuote

      else if word == 0x5C {- \ -} then
        case eatEscape (plusPtr pos 1) end row col of
          EscapeOk delta chunk ->
            let !newPos = plusPtr pos delta in
            chompChar newPos end row (col + fromIntegral delta) (numChars + 1) chunk

          EscapeProblem r c badEscape ->
            CharEscape r c badEscape

          EscapeEndOfFile ->
            CharEndless col

      else
        let
          !width = P.getCharWidth word
          !newPos = plusPtr pos width
        in
        chompChar newPos end row (col + 1) (numChars + 1) (ES.Slice pos width)



-- STRINGS


string :: (Row -> Col -> x) -> (E.String -> Row -> Col -> x) -> Parser x (ES.String, StringRepresentation)
string toExpectation toError =
  P.Parser $ \(P.State src pos end indent row col nl sn) cok _ cerr eerr ->
    if isDoubleQuote pos end then

      let
        !pos1 = plusPtr pos 1
      in
      case
        if isDoubleQuote pos1 end then
          let !pos2 = plusPtr pos 2 in
          if isDoubleQuote pos2 end then
            let
              !pos3 = plusPtr pos 3
              !col3 = col + 3
            in
            (multiString pos3 end row col3 pos3 row col mempty, TripleQuotedString)
          else
            (Ok pos2 row (col + 2) Utf8.empty, SingleQuotedString)
        else
          (singleString pos1 end row (col + 1) pos1 mempty, SingleQuotedString)
      of
        (Ok newPos newRow newCol utf8, representation) ->
          let
            !newState =
              P.State src newPos end indent newRow newCol nl sn
          in
          cok (utf8, representation) newState

        (Err r c x, _) ->
          cerr r c (toError x)

    else
      eerr row col toExpectation


{-# INLINE isDoubleQuote #-}
isDoubleQuote :: Ptr Word8 -> Ptr Word8 -> Bool
isDoubleQuote pos end =
  pos < end && P.unsafeIndex pos == 0x22 {- " -}


data StringResult
  = Ok (Ptr Word8) Row Col !ES.String
  | Err Row Col E.String


finalize :: Ptr Word8 -> Ptr Word8 -> [ES.Chunk] -> ES.String
finalize start end revChunks =
  ES.fromChunks $ reverse $
    if start == end then
      revChunks
    else
      ES.Slice start (minusPtr end start) : revChunks


addChunk :: ES.Chunk -> Ptr Word8 -> Ptr Word8 -> [ES.Chunk] -> [ES.Chunk]
addChunk chunk start end revChunks =
  if start == end then
    chunk : revChunks
  else
    chunk : ES.Slice start (minusPtr end start) : revChunks



-- SINGLE STRINGS


singleString :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Ptr Word8 -> [ES.Chunk] -> StringResult
singleString pos end row col initialPos revChunks =
  if pos >= end then
    Err row col E.StringEndless_Single

  else
    let
      !word = P.unsafeIndex pos
    in
      if word == 0x22 {- " -} then
        Ok (plusPtr pos 1) row (col + 1) $
          finalize initialPos pos revChunks

      else if word == 0x0A {- \n -} then
        Err row col E.StringEndless_Single

      else if word == 0x27 {- ' -} then
        let !newPos = plusPtr pos 1 in
        singleString newPos end row (col + 1) newPos $
          addChunk singleQuote initialPos pos revChunks

      else if word == 0x5C {- \ -} then
        case eatEscape (plusPtr pos 1) end row col of
          EscapeOk delta chunk ->
            let !newPos = plusPtr pos delta in
            singleString newPos end row (col + fromIntegral delta) newPos $
              addChunk chunk initialPos pos revChunks

          EscapeProblem r c x ->
            Err r c (E.StringEscape x)

          EscapeEndOfFile ->
            Err row (col + 1) E.StringEndless_Single

      else
        let !newPos = plusPtr pos (P.getCharWidth word) in
        singleString newPos end row (col + 1) initialPos revChunks



-- MULTI STRINGS


multiString :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Ptr Word8 -> Row -> Col -> [ES.Chunk] -> StringResult
multiString pos end row col initialPos sr sc revChunks =
  if pos >= end then
    Err sr sc E.StringEndless_Multi

  else
    let !word = P.unsafeIndex pos in
    if word == 0x22 {- " -} && isDoubleQuote (plusPtr pos 1) end && isDoubleQuote (plusPtr pos 2) end then
      Ok (plusPtr pos 3) row (col + 3) $
        finalize initialPos pos revChunks

    else if word == 0x27 {- ' -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end row (col + 1) pos1 sr sc $
        addChunk singleQuote initialPos pos revChunks

    else if word == 0x0A {- \n -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end (row + 1) 1 pos1 sr sc $
        addChunk newline initialPos pos revChunks

    else if word == 0x0D {- \r -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end row col pos1 sr sc $
        addChunk carriageReturn initialPos pos revChunks

    else if word == 0x5C {- \ -} then
      case eatEscape (plusPtr pos 1) end row col of
        EscapeOk delta chunk ->
          let !newPos = plusPtr pos delta in
          multiString newPos end row (col + fromIntegral delta) newPos sr sc $
            addChunk chunk initialPos pos revChunks

        EscapeProblem r c x ->
          Err r c (E.StringEscape x)

        EscapeEndOfFile ->
          Err sr sc E.StringEndless_Multi

    else
      let !newPos = plusPtr pos (P.getCharWidth word) in
      multiString newPos end row (col + 1) initialPos sr sc revChunks



-- ESCAPE CHARACTERS


data EscapeResult
  = EscapeOk !Int ES.Chunk
  | EscapeEndOfFile
  | EscapeProblem Row Col E.Escape


eatEscape :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> EscapeResult
eatEscape pos end row col =
  if pos >= end then
    EscapeEndOfFile

  else
    case P.unsafeIndex pos of
      0x6E {- n -} -> EscapeOk 2 $ ES.AsciiChar 0x0A {- \n -}
      0x72 {- r -} -> EscapeOk 2 $ ES.AsciiChar 0x0D {- \r -}
      0x74 {- t -} -> EscapeOk 2 $ ES.AsciiChar 0x09 {- \t -}
      0x22 {- " -} -> EscapeOk 2 $ ES.AsciiChar 0x22 {- " -}
      0x27 {- ' -} -> EscapeOk 2 $ ES.AsciiChar 0x27 {- ' -}
      0x5C {- \ -} -> EscapeOk 2 $ ES.AsciiChar 0x5C {- \ -}
      0x78 {- x -} -> eatPre019Unicode (plusPtr pos 1) end row col
      0x75 {- u -} -> eatUnicode (plusPtr pos 1) end row col
      _            -> EscapeProblem row col E.EscapeUnknown


eatPre019Unicode :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> EscapeResult
eatPre019Unicode pos end row col =
  if pos >= end then
    EscapeProblem row col (E.BadUnicodeFormat 2)
  else
    let
      (# newPos, code #) = Number.chompHex pos end
      !numDigits = minusPtr newPos pos
    in
    if newPos >= end then
      EscapeProblem row col $ E.BadUnicodeFormat (2 + fromIntegral numDigits)
    else
      EscapeOk (2 + numDigits) $ ES.UnicodeChar code


eatUnicode :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> EscapeResult
eatUnicode pos end row col =
  if pos >= end || P.unsafeIndex pos /= 0x7B {- { -} then
    EscapeProblem row col (E.BadUnicodeFormat 2)
  else
    let
      !digitPos = plusPtr pos 1
      (# newPos, code #) = Number.chompHex digitPos end
      !numDigits = minusPtr newPos digitPos
    in
    if newPos >= end || P.unsafeIndex newPos /= 0x7D {- } -} then
      EscapeProblem row col $ E.BadUnicodeFormat (2 + fromIntegral (minusPtr newPos pos))

    else if code < 0 || 0x10FFFF < code then
      EscapeProblem row col $ E.BadUnicodeCode (3 + fromIntegral (minusPtr newPos pos))

    else if numDigits < 4 || 6 < numDigits then
      EscapeProblem row col $
        E.BadUnicodeLength
          (3 + fromIntegral (minusPtr newPos pos))
          numDigits
          code

    else
      EscapeOk (numDigits + 4) $ ES.UnicodeChar code


{-# NOINLINE singleQuote #-}
singleQuote :: ES.Chunk
singleQuote =
  ES.AsciiChar 0x27 {- ' -}


{-# NOINLINE doubleQuote #-}
doubleQuote :: ES.Chunk
doubleQuote =
  ES.AsciiChar 0x22 {- " -}


{-# NOINLINE newline #-}
newline :: ES.Chunk
newline =
  ES.AsciiChar 0x0A {- \n -}


{-# NOINLINE carriageReturn #-}
carriageReturn :: ES.Chunk
carriageReturn =
  ES.AsciiChar 0x0D {- \r -}


{-# NOINLINE placeholder #-}
placeholder :: ES.Chunk
placeholder =
  ES.UnicodeChar 0xFFFD {-replacement character-}
