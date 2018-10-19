module Parse.ParsecAdapter
( string
, (<|>)
, many
, many1
, choice
, option
)
where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (ord)
import qualified Data.Text as T
import Foreign.ForeignPtr (ForeignPtr)
import qualified Parse.Primitives.Internals as I
import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as E
import Data.Text.Encoding (encodeUtf8)
import Parse.Primitives.Internals (Parser(Parser), State(State), noError, oneOf)


toWord8 :: String -> [Word8]
toWord8 = B.unpack . encodeUtf8 . T.pack


string :: String -> Parser String
string str =
    Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
    case eatString (toWord8 str) fp offset terminal row col of
      Err err ->
        cerr err

      Ok newOffset newRow newCol ->
        let
          !newState = State fp newOffset terminal indent newRow newCol ctx
        in
        cok str newState noError

data Result
  = Err E.ParseError
  | Ok Int Int Int

eatString :: [Word8] -> ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Result
eatString str fp offset terminal row col =
  case str of
    [] -> Ok offset row col
    0x0A {- \n -} : _ -> error "eatString doesn't support matching '\\n'"
    h : t ->
      if offset >= terminal then
        Err noError
      else if h == I.unsafeIndex fp offset then
        eatString t fp (offset + 1) terminal row (col + 1)
      else
        Err noError

(<|>) :: Parser a -> Parser a -> Parser a
a <|> b =
  oneOf [ a, b ]

many1 :: Parser a -> Parser [a]
many1 (Parser parser) =
  Parser $ \initialState cok cerr eok eerr ->
    let
      parseFirst state err =
        parser state
            (\a newState e -> parseNext [a] newState e)
            (\           e -> cerr e)
            (\a newState _ -> error "many1 succeeded with empty parser")
            (\           e -> eerr e)
      -- help :: [a] -> State -> E.ParseError b
      parseNext acc state err =
        -- TODO: is the error passing correct? what do errors mean for parser success cases?
        parser state
          (\a newState e -> parseNext (a:acc) newState e)
          (\           e -> cerr e)
          (\a newState _ -> error "many1 succeeded with empty parser")
          (\           _ -> cok (reverse acc) state err)
    in
      parseFirst initialState noError

many :: Parser a -> Parser [a]
many (Parser parser) =
  Parser $ \initialState cok cerr eok eerr ->
    let
      -- help :: [a] -> State -> E.ParseError b
      parseNext acc state err =
        -- TODO: is the error passing correct? what do errors mean for parser success cases?
        parser state
          (\a newState e -> parseNext (a:acc) newState e)
          (\           e -> cerr e)
          (\a newState _ -> error "many succeeded with empty parser")
          (\           _ -> cok (reverse acc) state err)
    in
      parseNext [] initialState noError

choice :: [Parser a] -> Parser a
choice = oneOf

option :: a -> Parser a -> Parser a
option a parser =
  oneOf [parser, pure a]