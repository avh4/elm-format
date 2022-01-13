{-# LANGUAGE DataKinds #-}
module ElmFormat.Parse where

import Elm.Utils ((|>))
import AST.V0_16

import AST.Module (Module)
import Data.Coapplicative
import qualified Data.Text as Text
import ElmVersion ( ElmVersion )
import qualified Parse.Literal
import qualified Parse.Parse as Parse
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result
import Reporting.Annotation (Located)
import qualified AST.Module as Module
import qualified Parse.Module
import Data.Text (Text)
import AST.Structure (ASTNS)
import qualified Data.Indexed as I


parse :: ElmVersion -> Text -> Result.Result () Syntax.Error (Module [UppercaseIdentifier] (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelNK))
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


import' :: ElmVersion -> Text -> Either [Syntax.Error] Module.UserImport
import' elmVersion text =
    toEither $ Parse.parse (Text.unpack text) (Parse.Module.import' elmVersion)


-- TODO: can this be removed?
parseLiteral :: Text -> Result.Result () Syntax.Error LiteralValue
parseLiteral input =
     Parse.parse (Text.unpack input) Parse.Literal.literal
