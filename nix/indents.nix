{ mkDerivation, base, concatenative, mtl, parsec, stdenv }:
mkDerivation {
  pname = "indents";
  version = "0.3.3";
  sha256 = "16lz21bp9j14xilnq8yym22p3saxvc9fsgfcf5awn2a6i6n527xn";
  libraryHaskellDepends = [ base concatenative mtl parsec ];
  homepage = "http://patch-tag.com/r/salazar/indents";
  description = "indentation sensitive parser-combinators for parsec";
  license = stdenv.lib.licenses.bsd3;
}
