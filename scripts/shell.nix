{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  project = import ./default.nix { inherit nixpkgs compiler; };
in
  nixpkgs.pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = project.env.nativeBuildInputs ++ (with nixpkgs.pkgs.haskellPackages; [
      cabal-install
    ]);
  }

