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
      identifier = { name = "elm-format-markdown"; version = "0.0.0.1"; };
      license = "BSD-3-Clause";
      copyright = "See https://github.com/avh4/elm-format/blob/master/LICENSE";
      maintainer = "gruen0aermel@gmail.com";
      author = "Aaron VonderHaar";
      homepage = "";
      url = "";
      synopsis = "Markdown parsing for Elm documentation comments";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../elm-format-markdown; }
