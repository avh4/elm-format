module Check exposing
    ( claim, claimTrue, claimFalse
    , quickCheck, check
    , suite
    , Claim, Evidence, UnitEvidence, SuccessOptions, FailureOptions
    , claim2True, claim2False, claim3, claim3True, claim3False, claim4, claim4True, claim4False, claim5, claim5True, claim5False
    , that, is, for, true, false
    )

{-| Property Based Testing module in Elm.


# Make a claim

@docs claim, claimTrue, claimFalse


# Check a claim

@docs quickCheck, check


# Group claims into a suite

@docs suite


# Types

@docs Claim, Evidence, UnitEvidence, SuccessOptions, FailureOptions


# Multi-arity claims

@docs claim2True, claim2False, claim3, claim3True, claim3False, claim4, claim4True, claim4False, claim5, claim5True, claim5False


# DSL

`elm-check` provides a shorthand DSL for authoring claims. The goal of this
DSL is to help improve readability and encode intent in the phrasing of your
test code.

With the DSL, claims read as either:

1.  claim - (string) - that - (actual) - is - (expected) - for - (investigator)
2.  claim - (string) - true - (predicate) - for - (investigator)
3.  claim - (string) - false - (predicate) - for - (investigator)

**Example:**

    claim_multiplication_by_one_noop =
        claim
            "Multiplying by one does not change a number"
            `true` (\n -> n * 1 == n)
            `for` int

    claim_reverse_append =
        claim
            "Append then reverse is equivalent to reverse then append"
            `that` (\( l1, l2 ) -> List.reverse (l1 ++ l2))
            `is` (\( l1, l2 ) -> List.reverse l1 ++ List.reverse l2)
            `for` tuple ( list int, list int )

It is important to note that, if you wish to deal with multi-arity functions
using this DSL, you must deal explicitly in tuples.

_Warning: The DSL follows a very strict format. Deviating from this format will
yield potentially unintelligible type errors. While not all of the type errors
are strictly necessary, they are there to ensure that the test is authored in
a uniform way. As a result, the following functions have horrendous type
signatures and you are better off ignoring them._

@docs that, is, for, true, false

-}

--------------------------
-- CORE LIBRARY IMPORTS --
--------------------------
-------------------------
-- THIRD PARTY IMPORTS --
-------------------------
-------------------
-- LOCAL IMPORTS --
-------------------

import Check.Investigator exposing (Investigator, tuple, tuple3, tuple4, tuple5)
import List
import Random exposing (Generator, Seed)
import Random.Extra as Random
import Trampoline exposing (Trampoline(..), trampoline)



-----------
-- TYPES --
-----------


{-| A Claim is an object that makes a claim of truth about a system.
A claim is either a function which yields evidence regarding the claim
or a list of such claims.
-}
type Claim
    = Claim String (Int -> Seed -> Evidence)
    | Suite String (List Claim)


{-| Evidence is the output from checking a claim or multiple claims.
-}
type Evidence
    = Unit UnitEvidence
    | Multiple String (List Evidence)


{-| UnitEvidence is the concrete type returned by checking a single claim.
A UnitEvidence can easily be converted to an assertion or can be considered
as the result of an assertion.
-}
type alias UnitEvidence =
    Result FailureOptions SuccessOptions


{-| SuccessOptions is the concrete type returned in case there is no evidence
found disproving a Claim.

SuccessOptions contains:

1.  the `name` of the claim
2.  the number of checks performed
3.  the `seed` used in order to reproduce the check.

-}
type alias SuccessOptions =
    { name : String
    , seed : Seed
    , numberOfChecks : Int
    }


{-| FailureOptions is the concrete type returned in case evidence was found
disproving a Claim.

FailureOptions contains:

1.  the `name` of the claim
2.  the minimal `counterExample` which serves as evidence that the claim is false
3.  the value `expected` to be returned by the claim
4.  the `actual` value returned by the claim
5.  the `seed` used in order to reproduce the results
6.  the number of checks performed
7.  the number of shrinking operations performed
8.  the original `counterExample`, `actual`, and `expected` values found prior
    to performing the shrinking operations.

-}
type alias FailureOptions =
    { name : String
    , counterExample : String
    , actual : String
    , expected : String
    , original :
        { counterExample : String
        , actual : String
        , expected : String
        }
    , seed : Seed
    , numberOfChecks : Int
    , numberOfShrinks : Int
    }



{- }
   encode_failureOptions : FailureOptions -> Value
   encode_failureOptions options =
     Encode.object
       [ ("name", Encode.string options.name)
       , ("counterExample", Encode.string options.counterExample)
       , ("actual", Encode.string options.actual)
       , ("expected", Encode.string options.expected)
       , ("original",
             Encode.object
               [ ("counterExample", Encode.string options.original.counterExample)
               , ("actual", Encode.string options.original.actual)
               , ("expected", Encode.string options.original.expected)
               ]

         )
       , ("seed",
           let (s0, s1) = case options.seed.state of
                 State s0 s1 -> (s0, s1)
           in
               Encode.list [Encode.int s0, Encode.int s1]
         )
       , ("numberOfChecks", Encode.int options.numberOfChecks)
       , ("numberOfShrinks", Encode.int options.numberOfShrinks)
       ]
-}
------------------
-- MAKE A CLAIM --
------------------


{-| Make a claim about a system.

    claim nameOfClaim actualStatement expectedStatement investigator

1.  The `nameOfClaim` is a string you pass in order to name your claim.
    This is very useful when trying to debug or reading reports.
2.  The `actualStatement` is a function which states something about your
    system. The result of which will be compared by equality `==` to the
    result of the `expectedStatement`.
3.  The `expectedStatement` is a function which states something which
    the `actualStatement` should conform to or be equivalent to. The result of
    which will be compared by equality `==` to the result of the `actualStatement`.
4.  The `investigator` is an investigator used to generate random values to be passed
    to the `actualStatement` and `expectedStatement` in order to attempt to
    disprove the claim. If a counter example is found, the `investigator` will then
    shrink the counter example until it yields a minimal counter example which
    is then easy to debug.

Example :

    claim_sort_idempotent =
        claim "Sort is idempotent"
            (\list -> List.sort (List.sort list))
            (\list -> List.sort list)
            (list int)

-}
claim : String -> (a -> b) -> (a -> b) -> Investigator a -> Claim
claim name actualStatement expectedStatement investigator =
    -------------------------------------------------------------------
    -- QuickCheck Algorithm with Shrinking :
    -- 1. Find a counter example within a given number of checks
    -- 2. If there is no such counter example, return a success
    -- 3. Else, shrink the counter example to a minimal representation
    -- 4. Return a failure.
    -------------------------------------------------------------------
    Claim name
    -- A Claim is just a function that takes a number of checks
    -- and a random seed and returns an `Evidence` object
    <|
        \numberOfChecks seed ->
            -- `numberOfChecks` is the given number of checks which is usually
            -- passed in by the `check` function. This sets an upper bound on
            -- the number of checks performed in order to find a counter example
            --
            -- `seed` is the random seed which is usually passed in by the `check`
            -- function. Explictly passing random seeds allow the user to reproduce
            -- checks in order to re-run old checks on newer, presumably less buggy,
            -- code.
            let
                -- Find the original counter example. The original counter example
                -- is the first counter example found that disproves the claim.
                -- This counter example, if found, will later be shrunk into a more
                -- minimal version, hence "original".
                --
                -- Note that since finding a counter example is a recursive process,
                -- trampolines are used. `originalCounterExample'` returns a
                -- trampoline.
                --
                -- originalCounterExample' : Seed -> Int -> Trampoline (Result (a, b, b, Seed, Int) Int)
                originalCounterExample' seed currentNumberOfChecks =
                    if currentNumberOfChecks >= numberOfChecks then
                        ------------------------------------------------------------------
                        -- Stopping Condition:
                        -- If we have checked the claim at least `numberOfChecks` times
                        -- Then we simple return `Ok` with the number of checks signifying
                        -- that we have failed to find a counter example.
                        ------------------------------------------------------------------
                        Done (Ok numberOfChecks)

                    else
                        let
                            --------------------------------------------------------------
                            -- Body of loop:
                            -- 1. We generate a new random value and the next seed using
                            --    the investigator's random generator and the previous seed.
                            -- 2. We calculate the actual outcome and the expected
                            --    outcome from the given `actualStatement` and
                            --    `expectedStatement` respectively
                            -- 3. We compare the actual and the expected
                            -- 4. If actual equals expected, we continue the loop with
                            --    the next seed and incrementing the current number of
                            --    checks
                            -- 5. Else, we have found our counter example.
                            --------------------------------------------------------------
                            ( value, nextSeed ) =
                                Random.generate investigator.generator seed

                            actual =
                                actualStatement value

                            expected =
                                expectedStatement value
                        in
                        if actual == expected then
                            Continue (\() -> originalCounterExample' nextSeed (currentNumberOfChecks + 1))

                        else
                            Done (Err ( value, actual, expected, nextSeed, currentNumberOfChecks + 1 ))

                -- originalCounterExample : Result (a, b, b, Seed, Int) Int
                originalCounterExample =
                    trampoline (originalCounterExample' seed 0)
            in
            case originalCounterExample of
                ------------------------------------------------------------
                -- Case: No counter examples were found
                -- We simply return the name of the claim, the seed, and the
                -- number of checks performed.
                ------------------------------------------------------------
                Ok numberOfChecks ->
                    Unit <|
                        Ok
                            { name = name
                            , seed = seed
                            , numberOfChecks = max 0 numberOfChecks
                            }

                ------------------------------------------------------------
                -- Case : A counter example was found
                -- We proceed to shrink the counter example to a more minimal
                -- representation which still disproves the claim.
                ------------------------------------------------------------
                Err ( originalCounterExample, originalActual, originalExpected, seed, numberOfChecks ) ->
                    let
                        ------------------------------------------------------------------
                        -- Find the minimal counter example:
                        -- 1. Given a counter example, we produce a list of values
                        --    considered more minimal (i.e. we shrink the counter example)
                        -- 2. We keep only the shrunken values that disprove the claim.
                        -- 3. If there are no such shrunken value, then we consider the
                        --    given counter example to be minimal and report the number
                        --    of shrinking operations performed.
                        -- 4. Else, we recurse, passing in the new shrunken value
                        --    and incrementing the current number of shrinks counter.
                        ------------------------------------------------------------------
                        --
                        -- Note that since finding the minimal counter example is a
                        -- recursive process, trampolines are used. `shrink` returns
                        -- a trampoline.
                        --
                        -- shrink : a -> Int -> Trampoline (a, Int)
                        shrink counterExample currentNumberOfShrinks =
                            let
                                -- Produce a list of values considered more minimal that
                                -- the given `counterExample`.
                                --
                                -- shrunkenCounterExamples : List a
                                shrunkenCounterExamples =
                                    investigator.shrinker counterExample

                                -- Keep only the counter examples that disprove the claim.
                                -- (i.e. they violate `actual == expected`)
                                --
                                -- failingShrunkenCounterExamples : List a
                                failingShrunkenCounterExamples =
                                    List.filter
                                        (\shrunk ->
                                            not (actualStatement shrunk == expectedStatement shrunk)
                                        )
                                        shrunkenCounterExamples
                            in
                            case List.head failingShrunkenCounterExamples of
                                Nothing ->
                                    --------------------------------------------------------
                                    -- Stopping Condition :
                                    -- If there are no further shrunken counter examples
                                    -- we simply return the given counter example and report
                                    -- the number of shrinking operations performed.
                                    --------------------------------------------------------
                                    Done ( counterExample, currentNumberOfShrinks )

                                Just failing ->
                                    --------------------------------------------------------
                                    -- Body of Loop :
                                    -- We simply recurse with the first shrunken counter
                                    -- example we can get our hands on and incrementing the
                                    -- current number of shrinking operations counter
                                    --------------------------------------------------------
                                    Continue (\() -> shrink failing (currentNumberOfShrinks + 1))

                        -- minimal : a
                        -- numberOfShrinks : Int
                        ( minimal, numberOfShrinks ) =
                            trampoline (shrink originalCounterExample 0)

                        -- actual : b
                        actual =
                            actualStatement minimal

                        -- expected : b
                        expected =
                            expectedStatement minimal
                    in
                    -- Here, we return an `Err` signifying that a counter example was
                    -- found. The returned record contains a number of fields and
                    -- values useful for diagnostics, such as the counter example,
                    -- the expected and the actual values, as well the original
                    -- unshrunk versions, the name of the claim, the seed used to
                    -- find the counter example, the number of checks performed to find
                    -- the counter example, and the number of shrinking operations
                    -- performed.
                    Unit <|
                        Err
                            { name = name
                            , seed = seed
                            , counterExample = toString minimal
                            , expected = toString expected
                            , actual = toString actual
                            , original =
                                { counterExample = toString originalCounterExample
                                , actual = toString originalActual
                                , expected = toString originalExpected
                                }
                            , numberOfChecks = numberOfChecks
                            , numberOfShrinks = numberOfShrinks
                            }


{-| Make a claim of truth about a system.

Similar to `claim`, `claimTrue` only considers claims which always yield `True`
to be true. If `claimTrue` manages to find an input which causes the given
predicate to yield `False`, then it will consider that as the counter example.

    claimTrue nameOfClaim predicate investigator

Example:

    claim_length_list_nonnegative =
        claimTrue "The length of a list is strictly non-negative"
            (\list -> List.length list >= 0)
            (list string)

-}
claimTrue : String -> (a -> Bool) -> Investigator a -> Claim
claimTrue name predicate =
    claim name predicate (always True)


{-| Make a claim of falsiness about a system.

Analogous to `claimTrue`, `claimFalse` only considers claims which always yield
`False` to be true. If `claimFalse` manages to find an input which causes the
given predicate to yield `True`, then it will consider that as the counter
example.

    claimFalse nameOfClaim predicate investigator

Example:

    claim_length_list_never_negative =
        claimFalse "The length of a list is never negative"
            (\list -> List.length list < 0)
            (list float)

-}
claimFalse : String -> (a -> Bool) -> Investigator a -> Claim
claimFalse name predicate =
    claim name predicate (always False)



-------------------
-- CHECK A CLAIM --
-------------------


{-| Check a claim.

To check a claim, you need to provide the number of checks which check will
perform as well a random seed. Given a random seed and a number of checks,
`check` will always yield the same result. Thus, `check` is especially useful
when you wish to reproduce checks.

    check claim 100 (Random.initialSeed 1)

-}
check : Claim -> Int -> Seed -> Evidence
check claim n seed =
    case claim of
        Claim name f ->
            f n seed

        Suite name claims ->
            Multiple name (List.map (\c -> check c n seed) claims)


{-| Quick check a claim.

This function is very useful when checking claims locally. `quickCheck` will
perform 100 checks and use `Random.initialSeed 1` as the random seed.

    quickCheck claim =
        check claim 100 (Random.initialSeed 1)

-}
quickCheck : Claim -> Evidence
quickCheck claim =
    check claim 100 (Random.initialSeed 1)



-------------------------------
-- GROUP CLAIMS INTO A SUITE --
-------------------------------


{-| Group a list of claims into a suite. This is very useful in order to
group similar claims together.

    suite nameOfSuite listOfClaims

-}
suite : String -> List Claim -> Claim
suite name claims =
    Suite name claims



------------------------
-- MULTI-ARITY CLAIMS --
------------------------


claim2 : String -> (a -> b -> c) -> (a -> b -> c) -> Investigator a -> Investigator b -> Claim
claim2 name actualStatement expectedStatement specA specB =
    claim name (\( a, b ) -> actualStatement a b) (\( a, b ) -> expectedStatement a b) (tuple ( specA, specB ))


claim2True : String -> (a -> b -> Bool) -> Investigator a -> Investigator b -> Claim
claim2True name predicate =
    claim2 name predicate (\_ _ -> True)


claim2False : String -> (a -> b -> Bool) -> Investigator a -> Investigator b -> Claim
claim2False name predicate =
    claim2 name predicate (\_ _ -> False)


claim3 : String -> (a -> b -> c -> d) -> (a -> b -> c -> d) -> Investigator a -> Investigator b -> Investigator c -> Claim
claim3 name actualStatement expectedStatement specA specB specC =
    claim name (\( a, b, c ) -> actualStatement a b c) (\( a, b, c ) -> expectedStatement a b c) (tuple3 ( specA, specB, specC ))


claim3True : String -> (a -> b -> c -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Claim
claim3True name predicate =
    claim3 name predicate (\_ _ _ -> True)


claim3False : String -> (a -> b -> c -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Claim
claim3False name predicate =
    claim3 name predicate (\_ _ _ -> False)


claim4 : String -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Claim
claim4 name actualStatement expectedStatement specA specB specC specD =
    claim name (\( a, b, c, d ) -> actualStatement a b c d) (\( a, b, c, d ) -> expectedStatement a b c d) (tuple4 ( specA, specB, specC, specD ))


claim4True : String -> (a -> b -> c -> d -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Claim
claim4True name predicate =
    claim4 name predicate (\_ _ _ _ -> True)


claim4False : String -> (a -> b -> c -> d -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Claim
claim4False name predicate =
    claim4 name predicate (\_ _ _ _ -> False)


claim5 : String -> (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e -> f) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Claim
claim5 name actualStatement expectedStatement specA specB specC specD specE =
    claim name (\( a, b, c, d, e ) -> actualStatement a b c d e) (\( a, b, c, d, e ) -> expectedStatement a b c d e) (tuple5 ( specA, specB, specC, specD, specE ))


claim5True : String -> (a -> b -> c -> d -> e -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Claim
claim5True name predicate =
    claim5 name predicate (\_ _ _ _ _ -> True)


claim5False : String -> (a -> b -> c -> d -> e -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Claim
claim5False name predicate =
    claim5 name predicate (\_ _ _ _ _ -> False)



---------
-- DSL --
---------


that : ((a -> b) -> (a -> b) -> Investigator a -> Claim) -> (a -> b) -> (a -> b) -> Investigator a -> Claim
that f x =
    f x


is : ((a -> b) -> Investigator a -> Claim) -> (a -> b) -> Investigator a -> Claim
is f x =
    f x


for : (Investigator a -> Claim) -> Investigator a -> Claim
for f x =
    f x


true : ((a -> Bool) -> (a -> Bool) -> Investigator a -> Claim) -> (a -> Bool) -> Investigator a -> Claim
true f pred =
    f pred (always True)


false : ((a -> Bool) -> (a -> Bool) -> Investigator a -> Claim) -> (a -> Bool) -> Investigator a -> Claim
false f pred =
    f pred (always False)
