{-# OPTIONS_GHC -Wall #-}
module Messages.Strings where

import Messages.Types (Message(..))


showFiles :: [FilePath] -> String
showFiles = unlines . map ((++) "    ")

renderMessage :: Message -> String

renderMessage ErrorsHeading = "ERRORS"
renderMessage ErrorFileLocation = "<location>"

renderMessage (FilesWillBeOverwritten filePaths) =
  unlines
    [ "This will overwrite the following files to use Elm’s preferred style:"
    , ""
    , showFiles filePaths
    , "This cannot be undone! Make sure to back up these files before proceeding."
    , ""
    , "Are you sure you want to overwrite these files with formatted versions? (y/n)"
    ]


renderMessage (NoElmFilesFound filePaths) =
  unlines
    [ "Could not find any .elm files on the specified paths:"
    , ""
    , showFiles filePaths
    , "Please check the given paths."
    ]

renderMessage CantWriteToOutputBecauseInputIsDirectory =
  unlines
    [ "Can't write to the OUTPUT path, because multiple .elm files have been specified."
    , ""
    , "Please remove the --output argument. The .elm files in INPUT will be formatted in place."
    ]

renderMessage ProcessingFile = "Processing file"
