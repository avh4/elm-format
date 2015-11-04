{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Parse where

import Elm.Utils ((|>))

import qualified AST.Module
import qualified Data.Text.Lazy as LazyText
import qualified Parse.Parse as Parse
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result
import qualified Reporting.Annotation as RA


parse :: LazyText.Text -> Result.Result () Syntax.Error AST.Module.Module
parse input =
    LazyText.unpack input
        |> Parse.parseSource


toMaybe :: Result.Result a b c -> Maybe c
toMaybe res =
    case res of
        Result.Result _ (Result.Ok c) ->
            Just c
        _ ->
            Nothing

toEither :: Result.Result a b c -> Either [b] c
toEither res =
    case res of
        Result.Result _ (Result.Ok c) ->
            Right c
        Result.Result _ (Result.Err b) ->
            Left $ map RA.drop b
