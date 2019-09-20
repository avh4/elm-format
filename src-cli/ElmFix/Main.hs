{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude hiding (readFile, writeFile)
import Data.Text.IO (readFile, writeFile)
import ElmFormat.Upgrade_0_19 (parseUpgradeDefinition, transformModule)
import ElmVersion

import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified Reporting.Result as Result
import qualified System.Environment

-- elm-fix <upgrade-definition> [FILES...]


main :: IO ()
main =
    do
        args <- System.Environment.getArgs
        case args of
            [ definitionFile, sourceFile ] ->
                do
                    definition <- parseUpgradeDefinition <$> readFile definitionFile

                    source <- Parse.parse Elm_0_19 <$> readFile sourceFile
                    case (definition, source) of
                        (Right def, Result.Result _ (Result.Ok ast)) ->
                            do
                                let upgraded = transformModule def ast
                                let output = Render.render Elm_0_19 upgraded
                                writeFile sourceFile output

                        (Left _, _) ->
                            error "TODO: couldn't parse upgrade definition"

                        (_, Result.Result _ (Result.Err _)) ->
                            error "TODO: couldn't parse source file"


            _ ->
                undefined

