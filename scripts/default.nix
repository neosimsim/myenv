{ pkgs, ghc ? "default" }:
let
  packages = import ./packages.nix { inherit pkgs ghc; };
in
with packages; pkgs.buildEnv {
  name = "scripts";
  paths = with packages; [
    posix
    go
    haskellPackages.scripts
  ];

  passthru = packages;
}

