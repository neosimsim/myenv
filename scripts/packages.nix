{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  hsPkgs = with nixpkgs.pkgs; if ghc == "default"
    then haskellPackages
    else haskell.packages.${ghc};
in
{
  haskellPackages = hsPkgs.extend (nixpkgs.haskell.lib.packageSourceOverrides {
    scripts = ./.;
  });
  posixScripts = nixpkgs.stdenv.mkDerivation {
    name = "scripts";
    src = ./.;
    buildPhase = "true";
    installPhase = "make PREFIX=$out install-posix";
  };
}

