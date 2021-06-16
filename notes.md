# GSOC Notes

## Notes on functions

    * Text.Parsec.Combinator.option

        Implemented exatly as it was by parsec

## Mapping between the parsec and Elm parser

`parsec` and `elm/compiler`'s parser very much operate on the same principles; continuation-passing style with four continuations in a parser: _empty ok_, _consumed ok_, _empty error_ and consumed error. `elm/compiler`'s parser if however less generic, it only parses bytestrings and has no concept of an "user" state. But `elm/compiler`s parser is general over the error type though, whereas `parsec` limits the user to string error messages

Here is what the declarations for the two parsers look like:

`parsec`. The type variables `s`, `u`, `m` and `a` represents _input data_, _user state_, _underlying monad_ and _output_ respectively.
```haskell
-- The general parsec type
newtype ParsecT s u m a =
  ParsecT (
    forall b .
      State s u
      -> (a -> State s u -> ParseError -> m b) -- consumed ok
      -> (a -> State s u -> ParseError -> m b) -- empty ok
      -> (ParseError -> m b)                   -- consumed err
      -> (ParseError -> m b)                   -- empty err
      -> m b

data State s u =
  State {
    stateInput :: s,
    statePos   :: !SourcePos,
    stateUser  :: !u
  }

data SourcePos = SourcePos SourceName Line Column

type SourceName = String
type Line = Int
type Column = Int


-- The specific instance of parsec parser used in elm-format
type IParser a = ParsecT String UserState UnderlyingMonad a

data UserState = UserState
  { newline :: [Bool]
  }

type UnderlyingMonad = Control.Monad.State SourcePos
```

`elm/compiler`:
```haskell
-- Elm
newtype Parser x a =
  Parser (
    forall b.
      State
      -> (a -> State -> b)                       -- consumed ok
      -> (a -> State -> b)                       -- empty ok
      -> (Row -> Col -> (Row -> Col -> x) -> b)  -- consumed err
      -> (Row -> Col -> (Row -> Col -> x) -> b)  -- empty err
      -> b
  )


data State =
  State
    { _src :: ForeignPtr Word8
    , _pos :: !(Ptr Word8)
    , _end :: !(Ptr Word8)
    , _indent :: !Word16
    , _row :: !Row
    , _col :: !Col
    }


type Row = Word16
type Col = Word16
```

### Mapping state

### Input
In `elm/compiler`'s parser, `_src`, `_pos` and `_end` are internals of the input bytestring to parse. For `IParser` the input is a `String`. Mapping between these datatypes is easy, all the is needed is for care to be taken that unicode data in `Char`'s isn't truncated when converting to `Word8` before creating a bytestring with `pack`.

### Location
mapping between `elm/compiler`'s `_row`&`_col` the `Line`&`Column` in `parsec`'s `statePos :: SourcePos` is trivial. However, the `SourcePos` also stores a `SourceName`. I doubt that this is every used by `elm-format`, but if it is then the wrapper layer can store that information instead.

### Indentation
`elm-format` uses `indents` for indentation aware parsing, which stores indentation information in the _underlying monad_, i.e. this bit `Control.Monad.State SourcePos`. `elm/compiler` stores indentation in `_indent`. The problem arises of how to map `Word16` to `SourcePos`.. Turns out that while `indents` does make use of both `Line` and `Column` in `SourcePos`, the functions actually used by `elm-format` only ever cares about `Column`, which is exactly what `_indent` maps to. So this works out as well!

### User state
The user state in `elm-format` is the `newline :: [Bool]` bit, and I don't understand what that means. Will have to ask Aaron that.
