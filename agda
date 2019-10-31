#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz
#!nix-shell --pure -i bash -p "haskellPackages.ghcWithPackages (ps: [ps.Agda])"

agda "$@"
