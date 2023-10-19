{ pkgs, config, lib, ... }: with lib;
let
  ghc = pkgs.haskellPackages.ghc.withHoogle (p: with p; [
    # Preinstall zlib to help cabal. Otherwise builds will
    # complain about missing zlib.h.
    zlib

    neosimsim-shell

    # Quality of life libraries for ghci
    aeson
    containers
    extra
    filelock
    filepath
    flow
    lens
    lens-aeson
    pidfile
    optparse-applicative
    typed-process
    QuickCheck
    raw-strings-qq
    regex-tdfa
    string-interpolate
    wreq

    # Preinstall common used lib to speed up nix builds.
    bifunctors
    concurrency
    dejafu
    hspec
    markdown-unlit
    profunctors
    safe
    semigroupoids
  ]);
in
{
  options = {
    myenv.ghc.enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.myenv.ghc.enable {
    home.packages = [
      ghc
    ] ++ (with pkgs; [
      haskell-language-server
    ]) ++ (with pkgs.haskellPackages; [
      apply-refact
      cabal2nix
      cabal-fmt
      cabal-install
      hindent
      hlint
      ormolu
      stylish-haskell
    ]);

    home.file.".ghci".source = ../../dotfiles/ghci;

    # workaround for haskell, due to w^x on OpenBSD
    home.shellAliases = lib.mkIf pkgs.stdenv.isOpenBSD {
      cabal = "env TMPDIR=/usr/local/cabal/build/ cabal";
    };
  };
}
