module Util.List where

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
        (first:rest) ->
            fn first
              : (pairs list
                  |> concatMap (\(a,b) -> spacer a b ++ [fn b]))
