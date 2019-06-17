module Parse.ParsecAdapter
  ( string
  , (<|>)
  , (<?>)
  , many
  , many1
  , choice
  , option, optionMaybe
  , satisfy
  , char, anyChar
  , eof
  , lookAhead
  , notFollowedBy
  , anyWord8
  )
  where

{-| This module implements parts of Parsec's API in terms of the new Elm 0.19 parser primitives
(`Parse.Primitives.Internals`).
Eventually the rest of the elm-format parsers should be rewritten to more closely match the
Elm 0.19 parser, and once that is done, this module can be removed.
-}

import Data.Word (Word8)
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as C
-- import Data.Char (ord)
import qualified Data.Char as Char
import qualified Data.Text as T
import Foreign.ForeignPtr (ForeignPtr)
-- import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as E
import Data.Text.Encoding (encodeUtf8)
import Parse.Primitives.Internals (Parser(Parser), State(State), unsafeIndex, noError, oneOf)
import Parse.Primitives (endOfFile)


toWord8 :: String -> [Word8]
toWord8 = B.unpack . encodeUtf8 . T.pack


string :: String -> Parser String
string str =
    Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
        case eatWord8s (toWord8 str) fp offset terminal row col of
            Err err ->
                cerr err

            Ok newOffset newRow newCol ->
                cok str (State fp newOffset terminal indent newRow newCol ctx) noError


data EatStringResult
    = Err E.ParseError
    | Ok Int Int Int


eatWord8s :: [Word8] -> ForeignPtr Word8 -> Int -> Int -> Int -> Int -> EatStringResult
eatWord8s str fp offset terminal row col =
    case str of
        [] -> Ok offset row col
        0x0A {- \n -} : _ -> error "eatWord8s doesn't support matching '\\n'"
        h : t ->
            if offset >= terminal then
                Err noError
            else if h == unsafeIndex fp offset then
                eatWord8s t fp (offset + 1) terminal row (col + 1)
            else
                Err noError


satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
    let
        (Parser p) = anyWord8
    in
    Parser $ \state cok cerr _ eerr ->
        p state
            (\a newState e ->
               let
                   c = Char.chr $ fromEnum a
               in
               if f c
                   then cok c newState e
                   else eerr noError
            )
            (\           e -> cerr e)
            (\_ _        _ -> error "satisfy got empty from anyWord8")
            (\           e -> eerr e)


char :: Char -> Parser Char
char c =
    Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
        case eatWord8s (toWord8 [c]) fp offset terminal row col of
            Err err ->
                cerr err

            Ok newOffset newRow newCol ->
                cok c (State fp newOffset terminal indent newRow newCol ctx) noError


anyChar :: Parser Char
anyChar =
    fmap (Char.chr . fromEnum) anyWord8


infix  0 <?>
(<?>) :: Parser a -> String -> Parser a
a <?> message =
    -- TODO: convert uses of <?> to how elm/compiler does things
    a

infixr 1 <|>
(<|>) :: Parser a -> Parser a -> Parser a
a <|> b =
  oneOf [ a, b ]


many1 :: Parser a -> Parser [a]
many1 (Parser parser) =
  Parser $ \initialState cok cerr _ eerr ->
    let
      parseFirst state _err =
        parser state
            (\a newState e -> parseNext [a] newState e)
            (\           e -> cerr e)
            (\_ _        _ -> error "many1 succeeded with empty parser")
            (\           e -> eerr e)

      -- help :: [a] -> State -> E.ParseError b
      parseNext acc state err =
        -- TODO: is the error passing correct? what do errors mean for parser success cases?
        parser state
          (\a newState e -> parseNext (a:acc) newState e)
          (\           e -> cerr e)
          (\_ _        _ -> error "many1 succeeded with empty parser")
          (\           _ -> cok (reverse acc) state err)
    in
    parseFirst initialState noError


many :: Parser a -> Parser [a]
many (Parser parser) =
  Parser $ \initialState cok cerr _ _ -> -- TODO: do we need to use eerr when the first term fails?
    let
      -- help :: [a] -> State -> E.ParseError b
      parseNext acc state err =
        -- TODO: is the error passing correct? what do errors mean for parser success cases?
        parser state
          (\a newState e -> parseNext (a:acc) newState e)
          (\           e -> cerr e)
          (\_ _        _ -> error "many succeeded with empty parser")
          (\           _ -> cok (reverse acc) state err)
    in
    parseNext [] initialState noError


choice :: [Parser a] -> Parser a
choice =
    oneOf


option :: a -> Parser a -> Parser a
option a parser =
    oneOf [parser, pure a]


optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe parser =
    option Nothing (fmap Just parser)


eof :: Parser ()
eof =
    endOfFile


lookAhead :: Parser a -> Parser a
lookAhead (Parser parser) =
    Parser $ \state cok cerr eok eer ->
        let
            cok' a _ e =
                cok a state e
        in
        parser state cok' cerr eok eer


notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser parser) =
    Parser $ \state _ _ eok eerr ->
        parser state
            (\_ _ _ -> eerr noError)
            (\_ -> eok () state noError)
            (\_ _ _ -> eerr noError)
            (\_ -> eok () state noError)


anyWord8 :: Parser Word8
anyWord8 =
    Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
        if offset >= terminal then
            eerr noError
        else
            case unsafeIndex fp offset of
                0x0A {- \n -} ->
                    cok 0x0A (State fp (offset + 1) terminal indent (row + 1) 1 ctx) noError

                0x0D {- \r -} ->
                    cok 0x0D (State fp (offset + 1) terminal indent row col ctx) noError

                0x09 {- \t -} ->
                    cerr (E.ParseError row col E.Tab)

                word ->
                    cok word (State fp (offset + 1) terminal indent row (col + 1) ctx) noError
