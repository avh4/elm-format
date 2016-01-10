module AllSyntax.BlockComments.Patterns (..) where


wildcard _ =
  ()


literal 1 2.0 "string" 'c' =
  ()


variable v =
  ()


unit () =
  ()


parens v =
  ()


tuple ( a, b ) =
  ()


emptyList [] =
  ()


list [ a, b ] =
  ()


consList (a :: b :: c) =
  ()


record { a, b } =
  ()


asAlias (() as x) =
  ()
