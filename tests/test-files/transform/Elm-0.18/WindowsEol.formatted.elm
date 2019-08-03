module AllSyntax.WindowsEol exposing (string)

{-| elm-format converts all CRLF line-endings to LF.

Raw CRs that are within strings will be escaped.

Escaped CRs will be retained.

-}


multilineString =
    """
"""


string =
    "\n"


rawCR =
    "\x0D"


escapedCR =
    "\x0D"
