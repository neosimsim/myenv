{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  packages = import ./packages.nix { inherit nixpkgs ghc; };
in
  with packages.haskellPackages;
  shellFor {
    packages = p: [p.scripts];
    buildInputs = map nixpkgs.haskell.lib.justStaticExecutables [
      cabal-install
      steeloverseer
      hlint
    ];
  }
