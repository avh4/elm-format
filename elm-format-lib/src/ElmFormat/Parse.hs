{-# LANGUAGE DataKinds #-}
module ElmFormat.Parse where

import Elm.Utils ((|>))
import AST.V0_16

import Data.Coapplicative
import qualified Data.Text as Text
import ElmVersion ( ElmVersion )
import qualified Parse.Literal
import qualified Parse.Parse as Parse
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result
import qualified Parse.Module
import Data.Text (Text)
import Parse.IParser (IParser, ParsedAST)
import qualified Parse.Helpers


parse :: ElmVersion -> Text -> Result.Result () Syntax.Error (ParsedAST 'ModuleNK)
parse elmVersion input =
    Text.unpack input
        |> Parse.parseModule elmVersion


parse' :: IParser a -> Text -> Either [Syntax.Error] a
parse' parser text =
        toEither $ Parse.parse (Text.unpack text) parser


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


import' :: ElmVersion -> Text -> Either [Syntax.Error] (C1 'BeforeTerm [UppercaseIdentifier], ParsedAST 'ImportMethodNK)
import' elmVersion =
    parse' (Parse.Module.import' elmVersion)


ref :: ElmVersion -> Text -> Either [Syntax.Error] (Ref [UppercaseIdentifier])
ref elmVersion =
    parse' (Parse.Helpers.var elmVersion)


-- TODO: can this be removed?
parseLiteral :: Text -> Result.Result () Syntax.Error LiteralValue
parseLiteral input =
     Parse.parse (Text.unpack input) Parse.Literal.literal
