module Data.Either.Extra (collectErrors, delimit) where

import Prelude ()
import Relude
import qualified Data.ReversedList as ReversedList
import qualified Data.List as List


collectErrors :: [Either l r] -> Either [l] [r]
collectErrors list =
    let
        step acc next =
            case (next, acc) of
                (Left l, Right _) ->
                    Left [l]

                (Left l, Left ls) ->
                    Left (l : ls)

                (Right r, Right rs) ->
                    Right (r : rs)

                (Right _, Left ls) ->
                    Left ls
    in
        foldl' step (Right []) list


{-| Could possibly be replaced by <https://hackage.haskell.org/package/utility-ht-0.0.16/docs/Data-List-HT.html#v:segmentBeforeRight>
-}
delimit :: [Either delim a] -> ([a], [ (delim, [a]) ])
delimit =
    let
        init =
            ( ReversedList.empty
            , Left ()
            )

        step (cur, state) (Right b) =
            ( ReversedList.push b cur
            , state
            )
        step (cur, state) (Left delim) =
            ( ReversedList.empty
            , case state of
                Left () ->
                    Right
                        ( delim
                        , ReversedList.empty
                        , ReversedList.toList cur
                        )
                Right (prev, secs, sec1) ->
                    Right
                        ( delim
                        , ReversedList.push (prev,ReversedList.toList cur) secs
                        , sec1
                        )
            )

        done (cur, Left ()) =
            ( ReversedList.toList cur
            , []
            )
        done (cur, Right (delim, secs, sec1)) =
            ( sec1
            , ReversedList.toList $ ReversedList.push (delim, ReversedList.toList cur) secs
            )
    in
    done . List.foldl' step init
