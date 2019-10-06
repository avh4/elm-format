module ElmRefactor.Cli (main) where

import Elm.Utils ((|>))

import CommandLine.Program (ProgramIO)
import CommandLine.TransformFiles (TransformMode(..))
import Data.Text (Text)
import ElmFormat.Upgrade_0_19 (UpgradeDefinition, parseUpgradeDefinition, transformModule)
import ElmFormat.World
import ElmRefactor.CliFlags as Flags
import ElmVersion
import Messages.Types

import qualified CommandLine.Program as Program
import qualified CommandLine.TransformFiles as TransformFiles
import qualified ElmFormat.Execute as Execute
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified Reporting.Result as Result


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


main' :: World m => Flags.Flags -> ProgramIO m String ()
main' flags =
    do
        let autoYes = True
        let run = Execute.run $ Execute.forHuman autoYes

        mode <- case Flags._input flags of
            [] -> Program.showUsage
            first:rest -> return $ FilesInPlace first rest

        let definitionFile = Flags._upgradeDefinition flags
        definition <- Program.liftME $ fmap (first (\() -> "Failed to parse upgrade definition")) $ parseUpgradeDefinition . snd <$> run (TransformFiles.readFromFile definitionFile)

        result <- Program.liftM $ run $ TransformFiles.applyTransformation (upgrade definition) mode
        if result
            then return ()
            else Program.failed


main :: World m => [String] -> m ()
main args =
    Program.run (Flags.parser "dev") id main' args
