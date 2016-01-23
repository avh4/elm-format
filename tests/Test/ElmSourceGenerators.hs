module Test.ElmSourceGenerators where

import Test.QuickCheck


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
    return $ "module Main " ++ exports ++ " where"


exportListing :: Gen String
exportListing =
  elements
    [ "(..)"
    , "(a, b, c)"
    , ""
    ]


declaration :: Gen String
declaration =
  do
    e <- expression
    return $ "foo = " ++ e


expression :: Gen String
expression =
  let
    unit = return "()"
    tuple = return "(a, b, c)"
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
