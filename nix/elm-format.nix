{ mkDerivation, ansi-terminal, ansi-wl-pprint, base, binary
, bytestring, Cabal, cmark, containers, coreutils, directory
, filepath, free, git, HUnit, indents, json, mtl
, optparse-applicative, parsec, process, QuickCheck, quickcheck-io
, split, stdenv, tasty, tasty-golden, tasty-hunit, tasty-quickcheck
, text
}:
mkDerivation {
  pname = "elm-format";
  version = "0.7.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [
    ansi-terminal ansi-wl-pprint base binary bytestring containers
    directory filepath free indents json mtl optparse-applicative
    parsec process split text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base cmark containers HUnit mtl parsec QuickCheck quickcheck-io
    split tasty tasty-golden tasty-hunit tasty-quickcheck text
  ];
  postInstall = ''
    ln -s $out/bin/elm-format-0.18 $out/bin/elm-format
  '';
  homepage = "http://elm-lang.org";
  description = "A source code formatter for Elm";
  license = stdenv.lib.licenses.bsd3;
  buildTools = [ git coreutils ];
}
