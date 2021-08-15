module Parse.ParsecAdapter.Message where


data Message
  = Message     !String -- raw message
  | Expect      !String -- expecting something
  | UnExpect    !String -- unexpected something
  deriving (Show, Eq)
