{-# OPTIONS_GHC -Wall #-}
module Messages.Strings where

import Messages.Types (Message(..))

renderMessage :: Message -> String

renderMessage ErrorsHeading = "ERRORS"
renderMessage ErrorFileLocation = "<location>"

renderMessage FollowingFilesWillBeOverwritten = "This will overwrite the following files to use Elm’s preferred style:"
renderMessage BackupFilesBeforeOverwriting = "This cannot be undone! Make sure to back up these files before proceeding."
renderMessage ConfirmOverwriting = "Are you sure you want to overwrite these files with formatted versions? (y/n)"

renderMessage NoElmFilesOnPath = "Could not find any .elm files on the specified path:"
renderMessage PleaseCheckPath = "Please check the given path."

renderMessage CantWriteToOutputBecauseInputIsDirectory = "Can't write to the OUTPUT path, because INPUT path is a directory."
renderMessage PleaseRemoveOutputArgument = "Please remove the OUTPUT argument. The .elm files in INPUT will be formatted in place."

renderMessage ProcessingFile = "Processing file"
