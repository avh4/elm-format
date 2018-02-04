{ mkDerivation, base, HUnit, QuickCheck, stdenv }:
mkDerivation {
  pname = "quickcheck-io";
  version = "0.1.4";
  sha256 = "6b3750590871b03908530764cdaa69ce67d5b514f533c1a4a6f4755f8267389d";
  libraryHaskellDepends = [ base HUnit QuickCheck ];
  homepage = "https://github.com/hspec/quickcheck-io#readme";
  description = "Use HUnit assertions as QuickCheck properties";
  license = stdenv.lib.licenses.mit;
}
