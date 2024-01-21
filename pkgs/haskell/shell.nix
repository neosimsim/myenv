{ pkgs ? import <nixpkgs> { }, ghc ? "default" }:
let
  packages = import ./. { inherit pkgs ghc; };
in
with packages.haskellPackages;
shellFor {
  packages = p: [ p.scripts ];
  withHoogle = true;
  buildInputs = map pkgs.haskell.lib.justStaticExecutables [
    cabal-install
    ormolu
    hlint
  ];
}
