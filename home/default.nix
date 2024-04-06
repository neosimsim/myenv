{ pkgs, config, lib, inputs, ... }: with lib;
let
  aspell = pkgs.aspellWithDicts (p: with p; [ en de ]);
in
{
  options.myenv = {

    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  imports = [
    inputs.plasma-manager.homeManagerModules.plasma-manager

    ./chromium.nix
    ./darwin.nix
    ./emacs
    ./firefox.nix
    ./ghc.nix
    ./git
    ./linux.nix
    ./plasma.nix
    ./wayland.nix
    ./gui-support.nix
    ./tmux
  ];

  config = mkIf config.myenv.enable {
    nix = {
      # The HomeManager NixOS module sets nix.package, so we override to use
      # the same configuration for NixOS and non-NixOS.
      package = lib.mkForce pkgs.nix;
      settings.experimental-features = [
        "nix-command"
        "flakes"
      ];
    };

    home = {
      packages = with pkgs; [
        age
        aspell
        cabal-shell
        rustup
        clisp
        dhall
        dhall-json
        entr
        fd
        fzf
        gcc
        git-lfs
        gnumake
        go
        gosec
        gotools
        htop
        isync
        jq
        libarchive
        nil
        nixpkgs-fmt
        nix-prefetch-scripts
        passage
        plan9port
        ripgrep
        texlive-full
        typespeed
        unzip
        utils-go
        utils-haskell
        utils-scripts
      ] ++ (with elmPackages; [
        elm
        elm-format
        elm-language-server
        elm-review
        elm-test
      ]) ++ (with haskellPackages; [
        hconv
        hookmark
        pandoc
      ]) ++ (with python3Packages; [
        # tools for emacs' elpy
        flake8

        black
        pylint
      ]);

      sessionVariables = {
        VISUAL = "$EDITOR";
        FCEDIT = "$EDITOR";
        CDPATH = ".:$HOME:$HOME/src";
        GOOS = "linux";
        GOARCH = "amd64";
        GOBIN = "$HOME/bin";
        FZF_DEFAULT_COMMAND = "fd --type file --follow --hidden --exclude .git";
        FZF_CTRL_T_COMMAND = "$FZF_DEFAULT_COMMAND";
      } // (lib.optionalAttrs (! config.myenv.guiSupport) {
        EDITOR = "emacsclient -ca  ''";
      });

      # don't use sessionPath because I want to prefix PATH
      sessionVariablesExtra = ''
        PATH=$HOME/bin:$PATH
        export PATH
      '';
    };

    programs.fish = {
      enable = true;
      shellInit = ''
        set -U fish_greeting
      '';
    };
  };
}
