{-# OPTIONS_GHC -Wall #-}

module ElmFix.Cli (main) where

import Prelude hiding (readFile, writeFile)
import Elm.Utils ((|>))

import CommandLine.TransformFiles (TransformMode(..))
import Control.Monad.Free
import Data.Text (Text)
import ElmFormat.FileStore (FileStore)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InputConsole (InputConsole)
import ElmFormat.OutputConsole (OutputConsole)
import ElmFormat.Upgrade_0_19 (UpgradeDefinition, parseUpgradeDefinition, transformModule)
import ElmFormat.World
import ElmVersion
import Messages.Formatter.Format
import Messages.Types

import qualified CommandLine.TransformFiles as TransformFiles
import qualified ElmFormat.Execute as Execute
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified Reporting.Result as Result

-- elm-fix <upgrade-definition> [FILES...]


upgrade :: UpgradeDefinition -> (FilePath, Text) -> Either InfoMessage Text
upgrade upgradeDefinition (_, inputText) =
    let
        elmVersion = Elm_0_19
    in
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok ast) ->
            transformModule upgradeDefinition ast
                |> Render.render elmVersion
                |> Right

        Result.Result _ (Result.Err _ ) ->
            -- TODO: return an error message
            error "TODO: couldn't parse source file"


main' ::
  (InputConsole f, OutputConsole f, InfoFormatter f, FileStore f, FileWriter f) =>
  [String] -> Free f Bool
main' args =
    do
        case args of
            definitionFile : first : rest ->
                do
                    definition <- parseUpgradeDefinition . snd <$> TransformFiles.readFromFile definitionFile

                    case definition of
                        Right def ->
                            TransformFiles.applyTransformation (upgrade def) (FilesInPlace first rest)

                        Left _ ->
                            error "TODO: couldn't parse upgrade definition"

            _ ->
                error "TODO: usage info"




main :: World m => [String] -> m ()
main args =
    do
        let autoYes = True
        let run = Execute.run $ Execute.forHuman autoYes
        result <- run $ main' args
        if result
            then exitSuccess
            else exitFailure

