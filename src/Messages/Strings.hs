{-# OPTIONS_GHC -Wall #-}
module Messages.Strings where

import Messages.Types


showFiles :: [FilePath] -> String
showFiles = unlines . map ((++) "    ")


renderMessage :: Message -> String

renderMessage ErrorsHeading = "ERRORS"

renderMessage (FilesWillBeOverwritten filePaths) =
  unlines
    [ "This will overwrite the following files to use Elm's preferred style:"
    , ""
    , showFiles filePaths
    , "This cannot be undone! Make sure to back up these files before proceeding."
    , ""
    , "Are you sure you want to overwrite these files with formatted versions? (y/n)"
    ]


renderMessage (BadInputFiles filePaths) =
  unlines
    [ "There was a problem reading one or more of the specified input files:"
    , ""
    , unlines $ map ((++) "    ") $ map showInputMessage filePaths
    , "Please check the given paths."
    ]

renderMessage CantWriteToOutputBecauseInputIsDirectory =
  unlines
    [ "Can't write to the OUTPUT path, because multiple .elm files have been specified."
    , ""
    , "Please remove the --output argument. The .elm files in INPUT will be formatted in place."
    ]

renderMessage (ProcessingFile file) =
    "Processing file " ++ file

renderMessage (FileWouldChange file) =
    "File would be changed " ++ file

renderMessage TooManyInputSources =
    "Too many input sources! Please only provide one of either INPUT or --stdin"


showInputMessage :: InputFileMessage -> String
showInputMessage (FileDoesNotExist path) =
    path ++ ": File does not exist"

showInputMessage (NoElmFiles path) =
    path ++ ": Directory does not contain any *.elm files"
