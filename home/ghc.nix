{ pkgs, config, lib, ... }: with lib;
let
  ghc = pkgs.haskellPackages.ghc.withHoogle (p: with p; [
    # Preinstall zlib to help cabal. Otherwise builds will
    # complain about missing zlib.h.
    zlib

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
    myenv.manageGhc = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.myenv.manageGhc {
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

    home.file.".ghci".text = ''
      :set prompt "λ "
      :set prompt-cont "| "
      :set +t

      :seti -XImportQualifiedPost
      :seti -XOverloadedStrings
      :seti -XQuasiQuotes
      :seti -XRankNTypes
      :seti -XTemplateHaskell
      :seti -XTypeApplications

      -- base
      import Control.Monad
      import Data.Foldable
      import Data.Function
      import Data.Functor
      import Data.Functor.Sum
      import Data.Int
      import Data.Monoid
      import Text.Read qualified

      -- regex-tdfa
      import Text.Regex.TDFA

      -- text
      import Data.Text.Lazy qualified as Text.Lazy
      import Data.Text.Lazy.IO qualified as Text.Lazy
      import Data.Text.Lazy.Encoding qualified as Text.Lazy
      import Data.Text qualified as Text
      import Data.Text.IO qualified as Text
      import Data.Text.Encoding qualified as Text

      -- lens
      import Control.Lens

      -- typed-process
      import System.Process.Typed

      -- raw-strings-qq
      import Text.RawString.QQ

      -- flow
      import Flow

      :def hoogle \s -> return $ ":! hoogle search -l --count=15 ̈" <> s <> "̈"
      :def! doc \s -> return $ ":! hoogle search -l --info ̈" <> s <> "̈"
    '';

    # workaround for haskell, due to w^x on OpenBSD
    home.shellAliases = lib.mkIf pkgs.stdenv.isOpenBSD {
      cabal = "env TMPDIR=/usr/local/cabal/build/ cabal";
    };
  };
}
