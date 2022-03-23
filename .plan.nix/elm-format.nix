{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "elm-format"; version = "0.8.5"; };
      license = "BSD-3-Clause";
      copyright = "See https://github.com/avh4/elm-format/blob/master/LICENSE";
      maintainer = "gruen0aermel@gmail.com";
      author = "Aaron VonderHaar";
      homepage = "https://elm-lang.org";
      url = "";
      synopsis = "A source code formatter for Elm";
      description = "A simple way to format your Elm code according to the official\nstyle guide.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      exes = {
        "elm-format" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."avh4-lib" or (errorHandler.buildDepError "avh4-lib"))
            (hsPkgs."elm-format-lib" or (errorHandler.buildDepError "elm-format-lib"))
            ];
          buildable = true;
          };
        };
      tests = {
        "elm-format-tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."avh4-lib" or (errorHandler.buildDepError "avh4-lib"))
            (hsPkgs."elm-format-lib" or (errorHandler.buildDepError "elm-format-lib"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-io" or (errorHandler.buildDepError "quickcheck-io"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."cmark" or (errorHandler.buildDepError "cmark"))
            (hsPkgs."elm-format-test-lib" or (errorHandler.buildDepError "elm-format-test-lib"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }
