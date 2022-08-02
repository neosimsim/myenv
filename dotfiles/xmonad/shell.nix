{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p;[
      xmonad
      xmonad-contrib
    ]))
  ];
}
