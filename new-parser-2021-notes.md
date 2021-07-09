# GSOC Notes

## About this document

Here are various notes I (`@emmabastas`) have made during the effort to integrate the parser from the Elm compiler into elm-format as part of a [GSOC project](https://github.com/elm-tooling/gsoc-projects/issues/13). Eventually, all of these notes will live elsewhere, like source code comments of github issues, but for now they are centralized in this document.

## Missing test coverage

* elm-format tests unicode in string literals, but unicode can appear in variable names and possibly other places. We need to figure exactly where and how unicode can appear and add test coverage for that.

* Should elm-format gracefully handle invalid utf-8? If so we might want test coverage for that.

    **UPDATE:** It's really uncomon for people to have invalid utf-8, and if so, they proably have bigger probles than elm-format crashing. For now, just crashig with a descriptive error message is enoug.

## Notes on timeline

As the timeline stands now, most of the time will be spent implementing the wrapper functions. After some more work, I don't think that this is how it will play out, I think we can be more ambitious.

Turns out there isn't as much to implement for the adapter layer as it might seem. `Text.Parsec.Char` only really consists of two major functions to implement, `string` and `satisfy`, all the other functions are implemented in terms of `satisfy`. Functions in `Text.Parsec.Combinator` are all implemented in terms of `Text.Parsec.Prim` functions. `Text.Parsec.Pos` and `Text.Parsec.Error` will essentially be copies of their respective parsec modules with minor changes. `Text.Parsec.Indent` probably won't do much either. So that leaves us with `Text.Parsec.Prim` containing the functions that need genuine implementations, apart from the _get*_, _set*_ and _update*_ style functions. This all leaves is with very few time consuming functions to implement. The one thing that has the potential of consuming a lot of time is if these foundational functions turns out to be very tricky to get right, with failing integration tests that could be to broad to help us pinpoint the bugs. Getting the first integration test to pass feels like to most difficult thing to do right now.

All this said, I think we can start to think about what we want to do with our time after the adapter layer has been implemented.

## Notes on implemented functions

* `Text.Parsec.Prim`
    * `Functor`, `Applicative` and `Monad` instances for `Parser`. Uses the underlying elm parser.

        The implementations simply unwrap to the underlying `elm/compiler` parser, applies the relevant function and re-wraps. So for this to work `parsec` and `elm/compiler` need to have the same idea of how these instances should behave, which they seem to do from looking at the code.

    * instance `Applicative.Alternative` for `Parser`. Implemented exactly as it was by parsec.

    * instance `MonadPlus` for `Parser`. Risks of bugs being introduced.

    * instance `Fail.MonadFail` for `Parser`. Implementation closely matches that of parsec.

        The implementation of `mplus` is straightforward except for the error merging behaviour in parsec, where if the tow parsers fails the errors are merged somehow. Not confident that I've got this right..

    * `<|>`. Implemented exactly as it was by parsec.

    * `<?>`. Dummy implementation.

    * `try`. Implemented exactly as it was by parsec.

    * `many`. Risk of bugs being introduced.

    * `skipMany`. Not to complicated.

    * `lookAhead`. Trivial implementation.

        The implementation of `many` is one of the more complex functions in parsec, and recursion is being used. Furthermore this implementation does not closely follow the original implementation.

    * `runParserT`. Closely follows the implementation of `Parse.Primitives.fromByteString`

        Difference being that `runParserT` converts the `String` to a `ByteString` first. Care has been taken that unicode points arent truncated when converting to `[Word8]` as an intermediate conversion step.

    * `getPosition`, `getState`, `updateState`. Trivial implementations.

* `Text.Parsec.Combinator`
    Almost all of the functions in this module are implemented in terms of functions found in `Text.Parsec.Prim`, and as such most of the functions are implemented exactly as they where by parserc (except for formatting changes). Only functions that somehow differ from the implementetion in parsec are listed.

    * `eof`. Not implemented the way parser does it. Implemented in a way that's more straightforward with the new parser. The error message is less descriptive.

        The elm parser fails if all input isn't consumed, which makes sense in the context of compiling Elm. parsec however defaults to succeeding even if everyting isn't consumed, and that behaviour is changed by `eof`. So as of right now, `eof` becommes a NoOp, maybe `Parse.Primitives` will have to be changed to not fail on unconsumed input (for elm-format it can still make sense to not parse all input) at some point, but not right not.

* `Text.Parsec.Char`
    The functions in this module all deal with `Char` (AKA unicode). There's really only two important functions here: `satisfy` wich consumes chars as long as a predicate holds, and `string` which succeed if a given string exatcly matches what is being consumed, and fails otherwise.  Some care has to be taken here because parsec deals with `Char`'s whereas the new parser deals with `Word8`'s. The current implementation handles valid utf-8, but not invalid utf-8.

* `Text.Parsec.Indent`
    * `indented`. Simple function.

        In indents `indented` function there is a `put $ setSourceLine s (sourceLine pos)` line which I don't really get. All of the functions from indents used by elm-format only care about the column of the reference, and not the row, so don't think this will be a problem.

    * `checkIndent`. Simple function.

    * `withPos`. Simple function, but I might have done in wrong..

        Also, what exactly does the `_indent` field represent? Does it represent indentation in terms of coulmns, or some multiple thereof? Should be easy enough to pinpont if there is an issure here anyhow.

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
