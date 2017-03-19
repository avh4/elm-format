module AllSyntax.WindowsEol exposing (..)

{-| elm-format converts all CRLF line-endings to LF.

CRs that are within strings will be escaped.
-}


multilineString =
    """\x0D
"""


string =
    "\x0D\n"
