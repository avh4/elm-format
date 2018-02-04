{ mkDerivation, ansi-wl-pprint, base, process, stdenv, transformers
, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.12.1.0";
  sha256 = "18b46d6d2c17e941bb02f84e980390f056795dce73ece946d71d3d4d002313d5";
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = stdenv.lib.licenses.bsd3;
}
