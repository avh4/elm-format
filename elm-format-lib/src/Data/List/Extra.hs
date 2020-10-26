module Data.List.Extra where

import Elm.Utils ((|>))
import qualified Data.List as List


pairs :: [a] -> [(a,a)]
pairs input =
    let
        step next ( prev, acc ) =
            case prev of
                Nothing ->
                    ( Just next, acc )
                Just prev' ->
                    ( Just next, ( next, prev' ) : acc )
    in
        List.foldr step (Nothing, []) input
            |> snd


intersperseMap :: (a -> a -> [b]) -> (a -> b) -> [a] -> [b]
intersperseMap spacer fn list =
    case list of
        [] -> []
        (first:_) ->
            fn first
              : (pairs list
                  |> concatMap (\(a,b) -> spacer a b ++ [fn b]))


shift :: a -> [(b,a)] -> ([(a,b)], a)
shift a list =
    let
        init = (a, [])
        step (holdA, acc) (nextB, nextA) =
            (nextA, (holdA, nextB) : acc)
        done (holdA, acc) =
            (List.reverse acc, holdA)
    in
        List.foldl step init list
            |> done
