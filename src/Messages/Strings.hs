{-# OPTIONS_GHC -Wall #-}
module Messages.Strings where

import Messages.Types


showFiles :: [FilePath] -> String
showFiles = unlines . map ((++) "    ")


showErrorMessage :: ErrorMessage -> String

showErrorMessage ErrorsHeading = "ERRORS"

showErrorMessage (FilesWillBeOverwritten filePaths) =
  unlines
    [ "This will overwrite the following files to use Elm's preferred style:"
    , ""
    , showFiles filePaths
    , "This cannot be undone! Make sure to back up these files before proceeding."
    , ""
    , "Are you sure you want to overwrite these files with formatted versions? (y/n)"
    ]


showErrorMessage (BadInputFiles filePaths) =
  unlines
    [ "There was a problem reading one or more of the specified input files:"
    , ""
    , unlines $ map ((++) "    ") $ map showInputMessage filePaths
    , "Please check the given paths."
    ]

showErrorMessage SingleOutputWithMultipleInputs =
  unlines
    [ "Can't write to the OUTPUT path, because multiple .elm files have been specified."
    , ""
    , "Please remove the --output argument. The .elm files in INPUT will be formatted in place."
    ]

showErrorMessage TooManyInputs =
    "Too many input sources! Please only provide one of either INPUT or --stdin"

showErrorMessage OutputAndValidate =
    "Cannot use --output and --validate together"

renderMessage (MustSpecifyVersionWithUpgrade elmVersion) =
    "I can only upgrade code to the latest Elm version.  To make sure I'm doing what you expect, you must also specify --elm-version=" ++ show elmVersion ++ " when you use --upgrade."

showInputMessage :: InputFileMessage -> String
showInputMessage (FileDoesNotExist path) =
    path ++ ": File does not exist"

showInputMessage (NoElmFiles path) =
    path ++ ": Directory does not contain any *.elm files"
