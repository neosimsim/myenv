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
    ./xserver.nix
    ./tmux
    ./plan9port.nix
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

    nixpkgs.overlays = with inputs; [
      self.overlays.default
      emacs-overlay.overlay
      nur.overlay
    ];

    home = {
      packages = with pkgs; [
        age
        aspell
        cabal-shell
        cargo-outdated
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
        pandoc
        passage
        ripgrep
        typespeed
        unzip
        utils-go
        utils-scripts
        utils-rust
      ] ++ (with haskellPackages; [
        hconv
        hookmark
      ]) ++ (with nodePackages; [
        typescript-language-server
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
      };

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
