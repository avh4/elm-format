{-# OPTIONS_GHC -Wall #-}
module Messages.Formatter.Format (InfoFormatter(..)) where


import Messages.Types


data InfoFormatter = InfoFormatter
  { onInfo :: InfoMessage -> IO ()
  }
