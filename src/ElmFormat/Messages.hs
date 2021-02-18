module ElmFormat.Messages (PromptMessage(..), InfoMessage(..), ErrorMessage(..)) where

-- inspired by:
-- https://wiki.haskell.org/Internationalization_of_Haskell_programs_using_Haskell_data_types

import Prelude ()
import Relude

import CommandLine.InfoFormatter (ToConsole(..), Loggable(..))
import CommandLine.ResolveFiles as ResolveFiles
import qualified Data.Text as Text
import qualified ElmFormat.Version
import ElmVersion
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax
import Reporting.Region (Region(..), Position(..))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))


data InfoMessage
  = ProcessingFile FilePath
  | FileWouldChange FilePath
  | ParseError FilePath [A.Located Syntax.Error]
  | JsonParseError FilePath Text


data PromptMessage
    = FilesWillBeOverwritten [FilePath]


data ErrorMessage
  = BadInputFiles [ResolveFiles.Error]
  | NoInputs
  | SingleOutputWithMultipleInputs
  | TooManyInputs
  | OutputAndValidate
  | MustSpecifyVersionWithUpgrade ElmVersion


showFiles :: [FilePath] -> Text
showFiles = unlines . fmap (\filename -> "    " <> Text.pack filename)


instance ToConsole PromptMessage where
    toConsole = \case
        FilesWillBeOverwritten filePaths ->
            unlines
                [ "This will overwrite the following files to use Elm's preferred style:"
                , ""
                , showFiles filePaths
                , "This cannot be undone! Make sure to back up these files before proceeding."
                , ""
                , "Are you sure you want to overwrite these files with formatted versions? (y/n)"
                ]


instance ToConsole InfoMessage where
    toConsole = \case
        ProcessingFile file ->
            "Processing file " <> Text.pack file

        FileWouldChange file ->
            "File would be changed " <> Text.pack file

        ParseError inputFile errs ->
            let
                location =
                    Text.pack $
                    case errs of
                        [] -> inputFile
                        (A.A (Region (Position line col) _) _) : _ -> inputFile ++ ":" ++ show line ++ ":" ++ show col
            in
            "Unable to parse file " <> location <> " To see a detailed explanation, run elm make on the file."

        JsonParseError inputFile err ->
            "Unable to parse JSON file " <> Text.pack inputFile <> "\n\n" <> err


instance Loggable InfoMessage where
    jsonInfoMessage elmVersion =
        let
            fileMessage filename message =
                Aeson.pairs $ mconcat
                    [ "path" .= (filename :: FilePath)
                    , "message" .= (message :: String)
                    ]
        in
        \case
        ProcessingFile _ -> Nothing
        FileWouldChange file ->
            Just $ fileMessage file $
                "File is not formatted with elm-format-" <> ElmFormat.Version.asString
                <> " --elm-version=" <> show elmVersion
        ParseError inputFile _ ->
            Just $ fileMessage inputFile "Error parsing the file"
        JsonParseError inputFile _ ->
            Just $ fileMessage inputFile "Error parsing the JSON file"


instance ToConsole ErrorMessage where
    toConsole = \case
        BadInputFiles filePaths ->
            unlines
                [ "There was a problem reading one or more of the specified INPUT paths:"
                , ""
                , unlines $ map ((<>) "    " . toConsole) filePaths
                , "Please check the given paths."
                ]

        SingleOutputWithMultipleInputs ->
            unlines
                [ "Can't write to the OUTPUT path, because multiple .elm files have been specified."
                , ""
                , "Please remove the --output argument. The .elm files in INPUT will be formatted in place."
                ]

        TooManyInputs ->
            "Too many input sources! Please only provide one of either INPUT or --stdin"

        OutputAndValidate ->
            "Cannot use --output and --validate together"

        MustSpecifyVersionWithUpgrade elmVersion ->
            "I can only upgrade code to specific Elm versions.  To make sure I'm doing what you expect, you must also specify --elm-version=" <> Text.pack (show elmVersion) <> " when you use --upgrade."

        NoInputs ->
            error "Error case NoInputs should be handled elsewhere.  Please report this issue at https://github.com/avh4/elm-format/issues"
