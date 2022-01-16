{ pkgs, ghc ? "default" }:
let
  packages = import ./packages.nix { inherit pkgs ghc; };
in with packages; pkgs.buildEnv {
  name = "scripts";
  paths = [
    posixScripts
    goScripts
    (pkgs.haskell.lib.justStaticExecutables haskellPackages.scripts)
  ];
}

