module ConvertRangeSyntax exposing (..)

withoutComments = [1..9]

withLeadingComments1 =
    [--A
    1
    ..
    --B
    9]


withLeadingComments2 =
    [{-A-}1 .. {-B-}9]

withComments =
    [{-A-}1{-B-}..{-C-}9{-D-}]
