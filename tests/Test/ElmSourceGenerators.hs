module Test.ElmSourceGenerators where

import Test.QuickCheck


wsRequired :: Gen String
wsRequired =
  elements
    [ " "
    , "\n "
    , "{-A-}"
    , "{-A-}{-B-}"
    , "--A\n "
    ]


wsOptional :: Gen String
wsOptional =
  oneof [ wsRequired, return "" ]


withWhitespace :: Gen String -> [String] -> Gen String
withWhitespace ws components =
  let
    step :: String -> Gen String -> Gen String
    step a b =
      do
        b' <- b
        ws' <- ws
        return $ a ++ ws' ++ b'
  in
    foldr step (return "") components


elmModule :: Gen String
elmModule =
  do
    header <- moduleHeader
    d <- declaration
    return $ unlines [ header, d ]


moduleHeader :: Gen String
moduleHeader =
  do
    exports <- exportListing
    withWhitespace wsRequired [ "module", "Main", "exposing", exports ]


exportListing :: Gen String
exportListing =
  oneof
    [ return "(..)"
    , withWhitespace wsOptional $ words "( a , b , c )"
    ]


declaration :: Gen String
declaration =
  do
    e <- expression
    withWhitespace wsOptional ["foo", "=", e]


expression :: Gen String
expression =
  let
    unit = withWhitespace wsOptional ["(", ")"]
    tuple = withWhitespace wsOptional $ words "( a , b , c )"
    literal =
      elements
        [ "1"
        , "2.0"
        , "'x'"
        , "\"ABC\""
        , "\"\"\"z\nz\n\"\"\""
        ]
    app =
      do
        e0 <- expression
        e1 <- expression
        e2 <- expression
        return $ unwords [ e0, e1, e2 ]
  in
    oneof
      [ unit, literal, app, tuple ]
