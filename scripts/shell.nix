{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  project = import ./default.nix { inherit nixpkgs ghc; };
in
  nixpkgs.pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = project.env.nativeBuildInputs ++ (with nixpkgs.pkgs.haskellPackages; [
      cabal-install
    ]);
  }

