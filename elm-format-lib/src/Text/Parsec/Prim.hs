{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Parsec.Prim
  ( unexpected
  , Parser(..)
  , (<?>)
  , (<|>)
  , lookAhead
  , try
  , many
  , skipMany
  , runParserT
  , getPosition
  , getInput
  , setInput
  , getState
  , updateState
  , getParserState
  , updateParserState
  ) where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail

import Text.Parsec.Pos (SourceName, SourcePos, newPos, sourceLine, sourceColumn)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Error as Error

import qualified Parse.Primitives as EP
import Parse.State (State(..))

import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import Data.Char (ord)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)


unknownError :: EP.Row -> EP.Col -> ParseError
unknownError row col =
  Error.newErrorUnknown "" row col


unexpected :: String -> Parser a
unexpected msg =
  EP.Parser $ \(EP.State _ _ _ _ row col _ _) _ _ _ eerr ->
      eerr row col (Error.newErrorMessage (Error.UnExpect msg) "TODO")


type Parser a = EP.Parser ParseError a


instance Applicative.Alternative (EP.Parser ParseError) where
    empty = mzero
    (<|>) = mplus


instance Fail.MonadFail (EP.Parser ParseError) where
    fail = parserFail


parserFail :: String -> Parser a
parserFail msg =
  EP.Parser $ \(EP.State _ _ _ _ row col sourceName _) _ _ _ eerr ->
    eerr row col $ Error.newErrorMessage (Error.Message msg) sourceName


instance MonadPlus (EP.Parser ParseError) where
  mzero = parserZero
  mplus = parserPlus


parserZero :: Parser a
parserZero =
  EP.Parser $ \state _ _ _ eerr ->
    let
      (EP.State _ _ _ _ row col _ _) = state
    in
    eerr row col unknownError


parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (EP.Parser p) (EP.Parser q) =
  EP.Parser $ \s cok eok cerr eerr ->
    let
      meerr r1 c1 toErr1 =
        let
          neerr r2 c2 toErr2 =
            let
              err = Error.mergeError (toErr1 r1 c1) (toErr2 r2 c2)
              row = fromIntegral $ sourceLine $ Error.errorPos err
              col = fromIntegral $ sourceColumn $ Error.errorPos err
            in
            eerr row col (\_ _ -> err)
        in q s cok eok cerr neerr
    in
    p s cok eok cerr meerr


infixr 1 <|>
infix  0 <?>


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus


(<?>) :: Parser a -> String -> Parser a
(<?>) p _ = p


lookAhead :: Parser a -> Parser a
lookAhead (EP.Parser p) =
    EP.Parser $ \s _ eok cerr eerr ->
      let
        eok' a _ = eok a s
      in
      p s eok' eok' cerr eerr

try :: Parser a -> Parser a
try (EP.Parser parser) =
  EP.Parser $ \s cok eok _ err ->
    parser s cok eok err err


many :: Parser a -> Parser [a]
many (EP.Parser p) =
  EP.Parser $ \s cok eok cerr eerr ->
    let
      many_ p' acc x s' =
        p
          s'
          (many_ p' (x:acc))
          parserDoesNotConsumeErr
          cerr
          (\_ _ _ -> cok (reverse (x:acc)) s')
    in
    p
      s
      (many_ (EP.Parser p) [])
      parserDoesNotConsumeErr
      cerr
      (\_ _ _ -> eok [] s)


-- Note that causing a runtime crash when using `many` with a parser that does
-- not consume is the same behaviour as it was with parsec
parserDoesNotConsumeErr = error "Text.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."


skipMany ::Parser a -> Parser ()
skipMany = undefined

runParserT :: Parser a -> State -> SourceName -> String -> Either ParseError a
runParserT (EP.Parser p) (State newline) name source =
  B.accursedUnutterablePerformIO $
    let
      (B.PS fptr offset length) = stringToByteString source
      !pos = plusPtr (unsafeForeignPtrToPtr fptr) offset
      !end = plusPtr pos length
      !result = p (EP.State fptr pos end 0 1 1 name newline) toOk toOk toErr toErr
    in
    do  touchForeignPtr fptr
        return result


toOk :: a -> EP.State -> Either x a
toOk !a _ =
  Right a


toErr :: EP.Row -> EP.Col -> (EP.Row -> EP.Col -> x) -> Either x a
toErr row col toError =
  Left (toError row col)


stringToByteString :: String -> B.ByteString
stringToByteString = B.pack . concatMap encodeChar


-- https://hackage.haskell.org/package/utf8-string-1.0.2/docs/src/Codec.Binary.UTF8.String.html#encodeChar
encodeChar :: Char -> [Word8]
encodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]


getPosition :: Parser SourcePos
getPosition =
  do  (EP.State _ _ _ _ row col sourceName _) <- getParserState
      return $ newPos sourceName row col


getInput :: Parser String
getInput = undefined

setInput :: String -> Parser ()
setInput = undefined


getState :: Parser State
getState =
  do  (EP.State _ _ _ _ _ _ _ newline) <- getParserState
      return (State newline)


updateState :: (State -> State) -> Parser ()
updateState f =
  do  updateParserState
        (\(EP.State src pos end indent row col sourceName newline) ->
          let
            (State newline') = f (State newline)
          in
          EP.State src pos end indent row col sourceName newline'
        )
      return ()


getParserState :: Parser EP.State
getParserState = updateParserState id


updateParserState :: (EP.State -> EP.State) -> Parser EP.State
updateParserState f =
  EP.Parser $ \s _ eok _ _ -> eok (f s) (f s)
