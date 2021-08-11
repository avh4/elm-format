module Parse.ParsecAdapter.Message where


import Data.Typeable (Typeable)


data Message
  = SysUnExpect !String -- @ library generated unexpect
  | UnExpect    !String -- @ unexpected something
  | Expect      !String -- @ expecting something
  | Message     !String -- @ raw message
  deriving ( Typeable )


instance Enum Message where
    fromEnum (SysUnExpect _) = 0
    fromEnum (UnExpect    _) = 1
    fromEnum (Expect      _) = 2
    fromEnum (Message     _) = 3
    toEnum _ = error "toEnum is undefined for Message"


instance Eq Message where
    m1 == m2 = fromEnum m1 == fromEnum m2


instance Ord Message where
    compare msg1 msg2 = compare (fromEnum msg1) (fromEnum msg2)
