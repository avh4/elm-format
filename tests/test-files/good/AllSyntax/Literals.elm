module AllSyntax.Literals (..) where


unit =
  ()


boolean =
  ( True
  , False
  )


int =
  1


hexadecimalInt =
  ( 0x00
  , 0xFF
  , 0x07FF
  , 0x00010000
  , 0x0000000100000000
  )


float =
  2.0


string =
  "string"


stringWithUnicodeCharacters =
  "a b'`<>{}/ڥ😀ぁ⾃𝟟"


stringWithSpecialEscapedCharacters =
  "\t\n\\\""


stringWithEscapedControlCharacters =
  ( "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x0B\x0C\x0D\x0E\x0F"
  , "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F"
  , "\x7F"
  )


stringWithWindowsWorkaroundCharacters =
  -- See https://github.com/avh4/elm-format/issues/134
  " "


stringWithEscapedNonPrintCharacters =
  "\x06DD\xFFFB\x110BD\xE007F"


stringWithWhitespace =
  " \x2000\x205F"


char =
  'c'


unicodeChars =
  [ 'a', ' ', '/', '"', 'ڥ', '😀', 'ぁ', '⾃', '𝟟' ]


escapedSpecialChars =
  [ '\t', '\n', '\\', '\'' ]


escapedControlChars =
  [ '\x00', '\x1F', '\x7F' ]


escapedNonPrintChars =
  [ '\x06DD', '\xFFFB', '\x110BD', '\xE007F' ]


whitespaceChars =
  [ ' ', '\x2000', '\x205F' ]


multilineString =
  """normals = "a b'`<>{}/ڥ😀ぁ⾃𝟟"
"\t\\
""
\"\"\"
\"\"\"\"
\x00\x01\x02\x03\x04\x05\x06\x07\x08\x0B\x0C\x0D\x0E\x0F
\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F
\x7F
\x06DD\xFFFB\x110BD\xE007F
 \x2000\x205F
"""
