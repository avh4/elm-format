module AllSyntax.Comments exposing (blockComment, indentedMultilineBlockComment, multilineCommentTrickDisabled, nestedBlockComment)


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
