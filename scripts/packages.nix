{ pkgs, ghc ? "default" }:
let
  hsPkgs = with pkgs; if ghc == "default"
    then haskellPackages
    else haskell.packages.${ghc};
in
{
  haskellPackages = hsPkgs.extend (pkgs.haskell.lib.packageSourceOverrides {
    scripts = (pkgs.lib.sourceByRegex ./. [
    "^.*\.md$"
    "^.*\.hs$"
    "^scripts\.cabal$"
    "^hsSrc.*$"
    "^hsTest.*$"
    "^hsMain.*$"
    ]);
  });
  posixScripts = pkgs.stdenv.mkDerivation {
    name = "scripts";
    src = ./.;
    buildPhase = "true";
    installPhase = ''
      sed -i '1c#!${pkgs.plan9port}/plan9/bin/rc' \
        Hsfmt

      make PREFIX=$out install-posix
    '';
  };
}
