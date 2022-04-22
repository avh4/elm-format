module ConvertRangeSyntax exposing (..)


withoutComments =
    List.range 1 9


withLeadingComments1 =
    List.range
        --A
        1
        --B
        9


withLeadingComments2 =
    List.range {- A -} 1 {- B -} 9


withComments =
    List.range ({- A -} 1 {- B -}) ({- C -} 9 {- D -})
