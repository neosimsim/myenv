{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  packages = import ./packages.nix { inherit nixpkgs ghc; };
in with packages; nixpkgs.buildEnv {
  name = "scripts";
  paths = [
    posixScripts
    (nixpkgs.haskell.lib.justStaticExecutables haskellPackages.scripts)
  ];
}

