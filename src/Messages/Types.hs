{-# OPTIONS_GHC -Wall #-}
module Messages.Types where

-- inspired by:
-- https://wiki.haskell.org/Internationalization_of_Haskell_programs_using_Haskell_data_types

data Message
  = ErrorsHeading
  | ErrorFileLocation

  | FollowingFilesWillBeOverwritten
  | BackupFilesBeforeOverwriting
  | ConfirmOverwriting

  | NoElmFilesOnPath
  | PleaseCheckPath

  | CantWriteToOutputBecauseInputIsDirectory

  | ProcessingFile
