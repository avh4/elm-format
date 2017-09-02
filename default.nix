{ mkDerivation, ansi-terminal, ansi-wl-pprint, base, binary
, bytestring, Cabal, cmark, containers, directory, filepath, free
, HUnit, indents, json, mtl, optparse-applicative, parsec, process
, QuickCheck, quickcheck-io, split, stdenv, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, text
, git-cli
}:
mkDerivation {
  pname = "elm-format";
  version = "0.7.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [
    ansi-terminal ansi-wl-pprint base binary bytestring containers
    directory filepath free indents json mtl optparse-applicative
    parsec process split text
  ];
  executableHaskellDepends = [ base ];
  executableToolDepends = [ git-cli ];
  testHaskellDepends = [
    base cmark containers HUnit mtl parsec QuickCheck quickcheck-io
    split tasty tasty-golden tasty-hunit tasty-quickcheck text
  ];
  homepage = "http://elm-lang.org";
  description = "A source code formatter for Elm";
  license = stdenv.lib.licenses.bsd3;
}
