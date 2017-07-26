module AllSyntax.Comments exposing (..)


blockComment =
    {- A -}
    ()


nestedBlockComment =
    {- A {- {- C -} B {- D -} -} {- E -} -}
    ()


indentedMultilineBlockComment =
    {- A
       B
       C
    -}
    ()


{--}
multilineCommentTrickDisabled =
    ()
--}



{--
multilineCommentTrickEnabled =
  ()
--}
