# GSOC Notes

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

        The implementation of `mplus` is straightforward except for the error merging behaviour in parsec, where if the tow parsers fails the errors are merged somehow. Not confident that I've got this right..

    * `<|>`. Implemented exactly as it was by parsec.

    * `<?>`. Dummy implementation.

    * `try`. Implemented exactly as it was by parsec.

    * `many`. Risk of bugs being introduced.

    * `lookAhead`. Trivial implementation.

        The implementation of `many` is one of the more complex functions in parsec, and recursion is being used. Furthermore this implementation does not closely follow the original implementation.

    * `runParserT`. Closely follows the implementation of `Parse.Primitives.fromByteString`

        Difference being that `runParserT` converts the `String` to a `ByteString` first. Care has been taken that unicode points arent truncated when converting to `[Word8]` as an intermediate conversion step.

    * `getPosition`, `getState`, `updateState`. Trivial implementations.

* `Text.Parsec.Combinator`
    * `choice`. Implemented exactly as it was by parsec.

    * `many1`. Implemented exactly as it was by parsec.

    * `option`. Implemented exactly as it was by parsec.

    * `optionMaybe`. Implemented exactly as it was by parsec.

    * `notFollowedBy`. Implemented exactly as it was by parsec.

    * `eof`. Dummy implementation.

        The elm parser fails if all input isn't consumed, which makes sense in the context of compiling Elm. parsec however defaults to succeeding even if everyting isn't consumed, and that behaviour is changed by `eof`. So as of right now, `eof` becommes a NoOp, maybe `Parse.Primitives` will have to be changed to not fail on unconsumed input (for elm-format it can still make sense to not parse all input) at some point, but not right not.

* `Text.Parsec.Char`
    The functions in this module all deal with `Char` (AKA unicode). There's really only two important functions here: `satisfy` wich consumes chars as long as a predicate holds, and `string` which succeed if a given string exatcly matches what is being consumed, and fails otherwise.

    Difficulties arrise by the fact that the new parser deals with input in terms of `Word8`'s, and one unicode `Char` can be represented by multiple `Word8`'s, so decoding and encoding might have to take pace. Currently, the implementation does not handle this, and runtime errors are thrown if unicode is encountered. Let's wait and see how elm-format uses `string` and `satisfy` first, for example if `string` is only used to match keywords, then we wont have to handle unicode in a nice way.

* `Text.Parsec.Indent`
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
