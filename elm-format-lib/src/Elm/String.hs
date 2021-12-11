-- This module is based on `Elm.String` in the Elm compiler
-- https://github.com/elm/compiler/blob/94715a520f499591ac6901c8c822bc87cd1af24f/compiler/src/Elm/String.hs

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, FlexibleInstances #-}
module Elm.String
  ( String
  , toChars
  , toBuilder
  , Chunk(..)
  , fromChunks
  )
  where


import Prelude hiding (String)
import Data.Binary (Binary, get, put)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString.Builder as B
import qualified Data.Utf8 as Utf8
import Data.Utf8 (MBA, newByteArray, copyFromPtr, freeze, writeWord8)
import GHC.Exts (RealWorld, Ptr)
import GHC.IO (stToIO, unsafeDupablePerformIO)
import GHC.ST (ST)
import GHC.Word (Word8)



-- STRINGS


type String =
  Utf8.Utf8 ELM_STRING


data ELM_STRING



-- HELPERS


toChars :: String -> [Char]
toChars =
  Utf8.toChars


{-# INLINE toBuilder #-}
toBuilder :: String -> B.Builder
toBuilder =
  Utf8.toBuilder



-- FROM CHUNKS


data Chunk
  = Slice (Ptr Word8) Int
  | AsciiChar Word8
  | CodePoint Int


fromChunks :: [Chunk] -> String
fromChunks chunks =
  unsafeDupablePerformIO (stToIO (
    do  let !len = sum (map chunkToWidth chunks)
        mba <- newByteArray len
        writeChunks mba 0 chunks
        freeze mba
  ))


chunkToWidth :: Chunk -> Int
chunkToWidth chunk =
  case chunk of
    Slice _ len   -> len
    AsciiChar _   -> 1
    CodePoint c -> unicodeCharToWidth c


-- Inspired by https://hackage.haskell.org/package/utf8-string-1.0.2/docs/src/Codec.Binary.UTF8.String.html#encodeChar
unicodeCharToWidth :: Int -> Int
unicodeCharToWidth c
  | c <= 0x7f   = 1
  | c <= 0x7ff  = 2
  | c <= 0xffff = 3
  | otherwise   = 4


writeChunks :: MBA RealWorld -> Int -> [Chunk] -> ST RealWorld ()
writeChunks mba offset chunks =
  case chunks of
    [] ->
      return ()

    chunk : chunks ->
      case chunk of
        Slice ptr len ->
          do  copyFromPtr ptr mba offset len
              let !newOffset = offset + len
              writeChunks mba newOffset chunks

        AsciiChar word ->
          do  writeWord8 mba offset word
              let !newOffset = offset + 1
              writeChunks mba newOffset chunks

        CodePoint code ->
          do  delta <- writeCode mba offset code
              let !newOffset = offset + delta
              writeChunks mba newOffset chunks


-- Inspired by https://hackage.haskell.org/package/utf8-string-1.0.2/docs/src/Codec.Binary.UTF8.String.html#encodeChar
writeCode :: MBA RealWorld -> Int -> Int -> ST RealWorld Int
writeCode mba offset c
  | c <= 0x7f     = do  writeWord8 mba offset (fromIntegral c)
                        return 1

  | c <= 0x7ff    = do  writeWord8 mba (offset + 0) (fromIntegral $ 0xc0 + (c `shiftR` 6))
                        writeWord8 mba (offset + 1) (fromIntegral $ 0x80 + c .&. 0x3f)
                        return 2

  | c <= 0xffff   = do  writeWord8 mba (offset + 0) (fromIntegral $ 0xe0 + (c `shiftR` 12))
                        writeWord8 mba (offset + 1) (fromIntegral $ 0x80 + ((c `shiftR` 6) .&. 0x3f))
                        writeWord8 mba (offset + 2) (fromIntegral $ 0x80 + c .&. 0x3f)
                        return 3

  | otherwise     = do  writeWord8 mba (offset + 0) (fromIntegral $ 0xf0 + (c `shiftR` 18))
                        writeWord8 mba (offset + 1) (fromIntegral $ 0x80 + ((c `shiftR` 12) .&. 0x3f))
                        writeWord8 mba (offset + 2) (fromIntegral $ 0x80 + ((c `shiftR` 6) .&. 0x3f))
                        writeWord8 mba (offset + 3) (fromIntegral $ 0x80 + c .&. 0x3f)
                        return 4



-- BINARY


instance Binary (Utf8.Utf8 ELM_STRING) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong
