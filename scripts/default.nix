{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  hsPkgs = with nixpkgs.pkgs; if ghc == "default" then haskellPackages else haskell.packages.${ghc};
  posixScripts = nixpkgs.stdenv.mkDerivation {
    name = "scripts";
    src = ./.;
    buildPhase = "true";
    installPhase = "make PREFIX=$out install-posix";
  };
in nixpkgs.buildEnv {
  name = "scripts";
  paths = [
    (nixpkgs.haskell.lib.justStaticExecutables (hsPkgs.callCabal2nix "scripts" ./. { }))
    posixScripts
  ];
}

