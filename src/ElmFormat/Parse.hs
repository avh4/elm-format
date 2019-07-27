{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Parse where

import Elm.Utils ((|>))
import AST.V0_16

import qualified AST.Module
import qualified Data.Text as Text
import ElmVersion
import qualified Parse.Literal
import qualified Parse.Parse as Parse
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result
import qualified Reporting.Annotation as RA


parse :: ElmVersion -> Text.Text -> Result.Result () Syntax.Error AST.Module.Module
parse elmVersion input =
    Text.unpack input
        |> Parse.parseModule elmVersion


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


parseLiteral :: Text.Text -> Result.Result () Syntax.Error Literal
parseLiteral input =
     Parse.parse (Text.unpack input) Parse.Literal.literal
