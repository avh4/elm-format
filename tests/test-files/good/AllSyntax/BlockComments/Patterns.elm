module AllSyntax.BlockComments.Patterns (..) where


type Foo
  = Foo () ()


wildcard _ =
  ()


literal 1 2.0 "string" 'c' =
  ()


variable v =
  ()


data (Foo {- Q -} x {- R -} y) =
  ()


unit ({- A -}) =
  ()


parens v =
  ()


tuple ( {- B -} a {- C -}, {- D -} b {- E -} ) =
  ()


emptyList [{- F -}] =
  ()


list [ {- G -} a {- H -}, {- I -} b {- J -} ] =
  ()


consList (a :: b :: c) =
  ()


record { {- K -} a {- L -}, {- M -} b {- N -} } =
  ()


asAlias (() {- O -} as {- P -} x) =
  ()
