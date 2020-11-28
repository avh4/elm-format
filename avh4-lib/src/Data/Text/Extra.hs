module Data.Text.Extra (longestSpanOf, LongestSpanResult(..)) where

import Elm.Utils ((|>))

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Maybe as Maybe


data LongestSpanResult
    = NoSpan
    | Span Int {- >= 1 -}
    deriving (Eq, Show)


longestSpanOf :: Char -> Text -> LongestSpanResult
longestSpanOf char input =
    case Text.foldl' step (Nothing, 0) input |> endCurrentSpan of
        0 -> NoSpan
        positive -> Span positive
    where
        step (currentSpan, longest) c =
            if c == char
                then
                    ( Just (1 + Maybe.fromMaybe 0 currentSpan)
                    , longest
                    )
                else
                    ( -- clear the current span
                      Nothing
                    , -- and update the longest
                      endCurrentSpan (currentSpan, longest)
                    )

        endCurrentSpan (Nothing, longest) = longest
        endCurrentSpan (Just current, longest) = max current longest
