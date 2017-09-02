{ mkDerivation, ansi-wl-pprint, base, process, stdenv, transformers
, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.12.1.0";
  sha256 = "1m8k4c04sg8xsx3fkv3krrfpjmphj01rhkpq0axl3s8p5innvd0q";
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = stdenv.lib.licenses.bsd3;
}
