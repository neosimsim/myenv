{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  hsPkgs = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};
  ghc = hsPkgs.ghcWithPackages (ps: with ps; [
          xmonad xmonad-contrib
        ]);
  hls = hsPkgs.haskell-language-server;
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc hls hsPkgs.ormolu hsPkgs.hlint ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}

