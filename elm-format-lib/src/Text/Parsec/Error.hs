module Text.Parsec.Error
  ( Message
  , ParseError
  , newErrorUnknown
  , errorPos
  , errorMessages
  ) where

import Data.List (nub, sort)
import Data.Typeable (Typeable)

import Text.Parsec.Pos (SourcePos)


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


messageString :: Message -> String
messageString (SysUnExpect s) = s
messageString (UnExpect    s) = s
messageString (Expect      s) = s
messageString (Message     s) = s


data ParseError = ParseError !SourcePos [Message]


newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos
    = ParseError pos []


errorPos :: ParseError -> SourcePos
errorPos = undefined

errorMessages :: ParseError -> [Message]
errorMessages = undefined


instance Show ParseError where
    show err
        = show (errorPos err) ++ ":" ++
          showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"
                           (errorMessages err)


showErrorMessages ::
    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map ("\n"++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1) = span ((SysUnExpect "") ==) msgs
      (unExpect,msgs2)    = span ((UnExpect    "") ==) msgs1
      (expect,messages)   = span ((Expect      "") ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages

      -- helpers
      showMany pre msgs3 = case clean (map messageString msgs3) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = separate ", " . clean

      separate   _ []     = ""
      separate   _ [m]    = m
      separate sep (m:ms) = m ++ sep ++ separate sep ms

      clean             = nub . filter (not . null)
