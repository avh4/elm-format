{ mkDerivation, ansi-wl-pprint, base, process, QuickCheck, stdenv
, transformers, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.13.2.0";
  sha256 = "18kcjldpzay3k3309rvb9vqrp5b1gqp0hgymynqx7x2kgv7cz0sw";
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = stdenv.lib.licenses.bsd3;
}
