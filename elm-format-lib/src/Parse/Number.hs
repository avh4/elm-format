-- This module is based on `Parse.Number` in the Elm compiler
-- https://github.com/elm/compiler/blob/94715a520f499591ac6901c8c822bc87cd1af24f/compiler/src/Parse/Number.hs

{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module Parse.Number
  ( Number(..)
  , number
  , Outcome(..)
  , chompInt
  , chompHex
  --, precedence
  )
  where


import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import qualified Data.Utf8 as Utf8

--import qualified AST.Utils.Binop as Binop
import Parse.Primitives (Parser, Row, Col)
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E

import AST.V0_16
import Data.Int (Int64)


-- HELPERS


isDirtyEnd :: Ptr Word8 -> Ptr Word8 -> Word8 -> Bool
isDirtyEnd pos end word =
  Var.getInnerWidthHelp pos end word > 0


--{-# INLINE isDecimalDigit #-}
isDecimalDigit :: Word8 -> Bool
isDecimalDigit word =
  word <= 0x39 {-9-} && word >= 0x30 {-0-}



---- NUMBERS


data Number
  = Int Int64 IntRepresentation
  | Float Double FloatRepresentation


number :: (Row -> Col -> x) -> (E.Number -> Row -> Col -> x) -> Parser x Number
number toExpectation toError =
  P.Parser $ \(P.State src pos end indent row col nl) cok _ cerr eerr ->
    if pos >= end then
      eerr row col toExpectation

    else
      let !word = P.unsafeIndex pos in
      if not (isDecimalDigit word) then
        eerr row col toExpectation

      else
        let
          outcome =
            if word == 0x30 {-0-} then
              chompZero (plusPtr pos 1) end
            else
              chompInt (plusPtr pos 1) end (fromIntegral (word - 0x30 {-0-}))
        in
          case outcome of
            Err newPos problem ->
              let
                !newCol = col + fromIntegral (minusPtr newPos pos)
              in
              cerr row newCol (toError problem)

            OkInt newPos n representation ->
              let
                !newCol = col + fromIntegral (minusPtr newPos pos)
                !integer = Int n representation
                !newState = P.State src newPos end indent row newCol nl
              in
              cok integer newState

            OkFloat newPos representation ->
              let
                !newCol = col + fromIntegral (minusPtr newPos pos)
                !value = parseFloat pos newPos
                !float = Float value representation
                !newState = P.State src newPos end indent row newCol nl
              in
              cok float newState


parseFloat :: Ptr Word8 -> Ptr Word8 -> Double
parseFloat pos end =
  read $ Utf8.toChars $ Utf8.fromPtr pos end



---- CHOMP OUTCOME


-- first Int is newPos
--
data Outcome
  = Err (Ptr Word8) E.Number
  | OkInt (Ptr Word8) Int64 IntRepresentation
  | OkFloat (Ptr Word8) FloatRepresentation



-- CHOMP INT


chompInt :: Ptr Word8 -> Ptr Word8 -> Int64 -> Outcome
chompInt !pos end !n =
  if pos >= end then

    OkInt pos n DecimalInt

  else

    let
      !word = P.unsafeIndex pos
    in
      if isDecimalDigit word then
        chompInt (plusPtr pos 1) end (10 * n + fromIntegral (word - 0x30 {-0-}))

      else if word == 0x2E {-.-} && not (isDot (plusPtr pos 1) end) then
        chompFraction pos end n

      else if word == 0x65 {-e-} || word == 0x45 {-E-} then
        chompExponent (plusPtr pos 1) end

      else if isDirtyEnd pos end word then
        Err pos E.NumberEnd

      else
        OkInt pos n DecimalInt


{-# INLINE isDot #-}
isDot :: Ptr Word8 -> Ptr Word8 -> Bool
isDot pos end =
  pos < end && P.unsafeIndex pos == 0x2E {-.-}



-- CHOMP FRACTION


chompFraction :: Ptr Word8 -> Ptr Word8 -> Int64 -> Outcome
chompFraction pos end n =
  let
    !pos1 = plusPtr pos 1
  in
  if pos1 >= end then
    Err pos (E.NumberDot n)

  else if isDecimalDigit (P.unsafeIndex pos1) then
    chompFractionHelp (plusPtr pos1 1) end

  else
    Err pos (E.NumberDot n)


chompFractionHelp :: Ptr Word8 -> Ptr Word8 -> Outcome
chompFractionHelp pos end =
  if pos >= end then
    OkFloat pos DecimalFloat

  else
    let !word = P.unsafeIndex pos in
    if isDecimalDigit word then
      chompFractionHelp (plusPtr pos 1) end

    else if word == 0x65 {-e-} || word == 0x45 {-E-} then
      chompExponent (plusPtr pos 1) end

    else if isDirtyEnd pos end word then
      Err pos E.NumberEnd

    else
      OkFloat pos DecimalFloat



-- CHOMP EXPONENT


chompExponent :: Ptr Word8 -> Ptr Word8 -> Outcome
chompExponent pos end =
  if pos >= end then
    Err pos E.NumberEnd

  else
    let !word = P.unsafeIndex pos in
    if isDecimalDigit word then
      chompExponentHelp (plusPtr pos 1) end

    else if word == 0x2B {-+-} || word == 0x2D {---} then

      let !pos1 = plusPtr pos 1 in
      if pos1 < end && isDecimalDigit (P.unsafeIndex pos1) then
        chompExponentHelp (plusPtr pos 2) end
      else
        Err pos E.NumberEnd

    else
      Err pos E.NumberEnd


chompExponentHelp :: Ptr Word8 -> Ptr Word8 -> Outcome
chompExponentHelp pos end =
  if pos >= end then
    OkFloat pos ExponentFloat

  else if isDecimalDigit (P.unsafeIndex pos) then
    chompExponentHelp (plusPtr pos 1) end

  else
    OkFloat pos ExponentFloat



-- CHOMP ZERO


chompZero :: Ptr Word8 -> Ptr Word8 -> Outcome
chompZero pos end =
  if pos >= end then
    OkInt pos 0 DecimalInt

  else
    let !word = P.unsafeIndex pos in
    if word == 0x78 {-x-} then
      chompHexInt (plusPtr pos 1) end

    else if word == 0x2E {-.-} then
      chompFraction pos end 0

    else if isDecimalDigit word then
      Err pos E.NumberNoLeadingZero

    else if isDirtyEnd pos end word then
      Err pos E.NumberEnd

    else
      OkInt pos 0 DecimalInt


chompHexInt :: Ptr Word8 -> Ptr Word8 -> Outcome
chompHexInt pos end =
  let (# newPos, answer #) = chompHex pos end in
  if answer < 0 then
    Err newPos E.NumberHexDigit
  else
    OkInt newPos answer HexadecimalInt



-- CHOMP HEX


-- Return -1 if it has NO digits
-- Return -2 if it has BAD digits

{-# INLINE chompHex #-}
chompHex :: (Integral i) => Ptr Word8 -> Ptr Word8 -> (# Ptr Word8, i #)
chompHex pos end =
  chompHexHelp pos end (-1) 0


chompHexHelp :: (Integral i) => Ptr Word8 -> Ptr Word8 -> i -> i -> (# Ptr Word8, i #)
chompHexHelp pos end answer accumulator =
  if pos >= end then
    (# pos, answer #)
  else
    let
      !newAnswer =
        stepHex pos end (P.unsafeIndex pos) accumulator
    in
    if newAnswer < 0 then
      (# pos, if newAnswer == -1 then answer else -2 #)
    else
      chompHexHelp (plusPtr pos 1) end newAnswer newAnswer


{-# INLINE stepHex #-}
stepHex :: (Integral i) => Ptr Word8 -> Ptr Word8 -> Word8 -> i -> i
stepHex pos end word acc
  | 0x30 {-0-} <= word && word <= 0x39 {-9-} = 16 * acc + fromIntegral (word - 0x30 {-0-})
  | 0x61 {-a-} <= word && word <= 0x66 {-f-} = 16 * acc + 10 + fromIntegral (word - 0x61 {-a-})
  | 0x41 {-A-} <= word && word <= 0x46 {-F-} = 16 * acc + 10 + fromIntegral (word - 0x41 {-A-})
  | isDirtyEnd pos end word                  = -2
  | True                                     = -1



-- PRECEDENCE


--precedence :: (Row -> Col -> x) -> Parser x Binop.Precedence
--precedence toExpectation =
--  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
--    if pos >= end then
--      eerr row col toExpectation
--
--    else
--      let !word = P.unsafeIndex pos in
--      if isDecimalDigit word then
--        cok
--          (Binop.Precedence (fromIntegral (word - 0x30 {-0-})))
--          (P.State src (plusPtr pos 1) end indent row (col + 1))
--
--      else
--        eerr row col toExpectation
