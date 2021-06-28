{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}

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
  ) where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail

import Text.Parsec.Pos (SourceName, SourcePos, newPos)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Error as Error

import qualified Parse.Primitives as EP
import Parse.State (State)

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
unexpected msg
    = Parser $ EP.Parser $ \(EP.State _ _ _ _ row col) _ _ _ eerr ->
      eerr row col (Error.newErrorMessage (Error.UnExpect msg) "TODO")



data Parser a
  = Parser (EP.Parser ParseError a)


instance Functor Parser where
    fmap f (Parser p) = Parser (fmap f p)


instance Applicative.Applicative Parser where
    pure = return
    (Parser f) <*> (Parser a) = Parser (f <*> a)

instance Applicative.Alternative Parser where
    empty = mzero
    (<|>) = mplus


instance Monad Parser where
    return = Parser . return
    (Parser p) >>= f = Parser $ p >>= unwrap . f

unwrap :: Parser a -> EP.Parser ParseError a
unwrap (Parser p) = p


instance Fail.MonadFail Parser where
    fail = undefined


instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus


parserZero :: Parser a
parserZero =
  Parser $ EP.Parser $ \state _ _ _ eerr ->
    let
      (EP.State _ _ _ _ row col) = state
    in
    eerr row col unknownError


parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (Parser p) (Parser q) =
  Parser $ EP.oneOf (\row col -> undefined) [p, q]


infixr 1 <|>
infix  0 <?>


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus


(<?>) :: Parser a -> String -> Parser a
(<?>) p _ = p


lookAhead :: Parser a -> Parser a
lookAhead = undefined


try :: Parser a -> Parser a
try (Parser (EP.Parser parser)) =
  Parser $ EP.Parser $ \s cok eok _ err ->
    parser s cok eok err err


many :: Parser a -> Parser [a]
many (Parser (EP.Parser p)) =
  Parser $ EP.Parser $ \s cok eok cerr eerr ->
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
runParserT (Parser (EP.Parser p)) _ name source =
  B.accursedUnutterablePerformIO $
    let
      (B.PS fptr offset length) = stringToByteString source
      !pos = plusPtr (unsafeForeignPtrToPtr fptr) offset
      !end = plusPtr pos length
      !result = p (EP.State fptr pos end 0 1 1) toOk toOk toErr toErr
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
getPosition = undefined

getInput :: Parser String
getInput = undefined

setInput :: String -> Parser ()
setInput = undefined

getState :: Parser State
getState = undefined

updateState :: (State -> State) -> Parser ()
updateState = undefined
