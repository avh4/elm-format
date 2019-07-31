module Main exposing (chars, multilineString, string)


string =
    "\u{0000}\u{000F}\u{000F}\u{001F}\u{06DD}\u{2000}\u{110BD}"


multilineString =
    """\u{0000}\u{000F}\u{000F}\u{001F}\u{06DD}\u{2000}\u{110BD}"""


chars =
    [ '\u{0000}', '\u{000F}', '\u{000F}', '\u{001F}', '\u{06DD}', '\u{2000}', '\u{110BD}' ]
