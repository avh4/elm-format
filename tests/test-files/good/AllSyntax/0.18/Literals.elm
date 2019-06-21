module AllSyntax.Literals exposing (boolean, char, escapedControlChars, escapedNonPrintChars, escapedSpecialChars, multilineString, multilineStringQuoteHandling, string, stringWithEscapedControlCharacters, stringWithEscapedNonPrintCharacters, stringWithSpecialEscapedCharacters, stringWithUnicodeCharacters, stringWithWhitespace, stringWithWindowsWorkaroundCharacters, unicodeChars, unit, whitespaceChars)


unit =
    ()


boolean =
    [ True
    , False
    ]


integer =
    [ 1
    , 0x0F1A
    , 0x7FFFFFFFFFFFFFFF
    , 9223372036854775807
    , 0x8000000000000000
    , -9223372036854775808
    ]


float =
    [ 2.0
    ]


string =
    "string"


stringWithUnicodeCharacters =
    "a b'`<>{}/ڥ😀ぁ⾃𝟟"


stringWithEscapedCharacterFollowedByHexDigit =
    "😀0😀9😀a😀F"


stringWithSpecialEscapedCharacters =
    "\t\n\\\""


stringWithEscapedControlCharacters =
    [ "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x0B\x0C\x0D\x0E\x0F"
    , "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F"
    , "\x7F"
    ]


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


multilineStringQuoteHandling =
    [ """trailing double quote\""""
    , """trailing double quote 2\"\""""
    , """trailing double quote 3\"\"\""""
    , """trailing double quote 4\"\"\"\""""
    , """embedded triple double quotes (\"\"\")"""
    , """embedded triple double quotes (\"\"\") (\"\"\"\")"""
    , """""leading double quote 2"""
    , """\"\"\" leading double quote 3"""
    , """\"\"\"\" leading double quote 4"""
    ]
