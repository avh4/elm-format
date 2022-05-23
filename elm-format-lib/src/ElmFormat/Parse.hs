module ElmFormat.Parse where

import AST.V0_16

import Data.Coapplicative
import ElmVersion ( ElmVersion )
import qualified Parse.Literal
import qualified Parse.Parse as Parse
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result
import qualified Parse.Module
import Parse.IParser (IParser, ParsedAST)
import qualified Parse.Helpers
import Data.ByteString (ByteString)


parse :: ElmVersion -> ByteString -> Result.Result () Syntax.Error (ParsedAST 'ModuleNK)
parse = Parse.parseModule


parse' :: IParser a -> ByteString -> Either [Syntax.Error] a
parse' parser text =
    toEither $ Parse.parse text parser


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


import' :: ElmVersion -> ByteString -> Either [Syntax.Error] (C1 'BeforeTerm [UppercaseIdentifier], ParsedAST 'ImportMethodNK)
import' elmVersion =
    parse' (Parse.Module.import' elmVersion)


ref :: ElmVersion -> ByteString -> Either [Syntax.Error] (Ref [UppercaseIdentifier])
ref elmVersion =
    parse' (Parse.Helpers.var elmVersion)


parseLiteral :: ByteString -> Result.Result () Syntax.Error LiteralValue
parseLiteral input =
     Parse.parse input Parse.Literal.literal
