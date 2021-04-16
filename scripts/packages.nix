{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  hsPkgs = with nixpkgs.pkgs; if ghc == "default"
    then haskellPackages
    else haskell.packages.${ghc};
in
{
  haskellPackages = hsPkgs.extend (nixpkgs.haskell.lib.packageSourceOverrides {
    scripts = (nixpkgs.lib.sourceByRegex ./. [
    "^.*\.md$"
    "^.*\.hs$"
    "^scripts\.cabal$"
    "^hsSrc.*$"
    "^hsTest.*$"
    "^hsMain.*$"
    ]);
  });
  posixScripts = nixpkgs.stdenv.mkDerivation {
    name = "scripts";
    src = ./.;
    buildInputs = [ nixpkgs.plan9port ];
    buildPhase = "true";
    installPhase = ''
      # ensure PLAN9 is in PATH but at the end so patchShebangs finds
      # rc(1) but uses GNU sed instead of plan9 sed.
      . 9; . u
      patchShebangs Hsfmt agofmt
      make PREFIX=$out install-posix
    '';
  };
}

