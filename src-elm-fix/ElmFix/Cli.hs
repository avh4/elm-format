{-# OPTIONS_GHC -Wall #-}

module ElmFix.Cli (main) where

import Prelude hiding (readFile, writeFile)
import ElmFormat.Upgrade_0_19 (parseUpgradeDefinition, transformModule)
import ElmFormat.World
import ElmVersion

import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified Reporting.Result as Result

-- elm-fix <upgrade-definition> [FILES...]


main :: World m => [String] -> m ()
main args =
    do
        case args of
            [ definitionFile, sourceFile ] ->
                do
                    definition <- parseUpgradeDefinition <$> readUtf8File definitionFile

                    source <- Parse.parse Elm_0_19 <$> readUtf8File sourceFile
                    case (definition, source) of
                        (Right def, Result.Result _ (Result.Ok ast)) ->
                            do
                                let upgraded = transformModule def ast
                                let output = Render.render Elm_0_19 upgraded
                                writeUtf8File sourceFile output

                        (Left _, _) ->
                            error "TODO: couldn't parse upgrade definition"

                        (_, Result.Result _ (Result.Err _)) ->
                            error "TODO: couldn't parse source file"


            _ ->
                undefined

