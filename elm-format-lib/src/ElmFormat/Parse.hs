{-# LANGUAGE DataKinds #-}
module ElmFormat.Parse where

import Elm.Utils ((|>))
import AST.V0_16

import AST.Module (Module)
import AST.Structure
import Data.Coapplicative
import qualified Data.Text as Text
import ElmVersion
import qualified Parse.Literal
import qualified Parse.Parse as Parse
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result
import Reporting.Annotation (Located)


parse :: ElmVersion -> Text.Text -> Result.Result () Syntax.Error (Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK))
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
            Left $ map extract b


-- TODO: can this be removed?
parseLiteral :: Text.Text -> Result.Result () Syntax.Error LiteralValue
parseLiteral input =
     Parse.parse (Text.unpack input) Parse.Literal.literal
