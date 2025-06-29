{ pkgs, config, lib, ... }:
let
  aspell = pkgs.aspellWithDicts (p: with p; [ en de ]);
in
{
  options = {
    myenv.coreutils = {
      enable = lib.mkEnableOption ''
        Install utils I can't live without.
      '';
    };
  };

  imports = [
    ./darwin.nix
    ./emacs
    ./ghc
    ./git
    ./linux.nix
    ./plasma.nix
    ./wayland.nix
    ./tmux
    ./plan9port.nix
    ./go.nix
    ./texlive.nix
    ./nushell
  ];

  config = lib.mkIf config.myenv.coreutils.enable {
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
        # (sort-lines nil (string-match "^" (buffer-string) (point)) (string-match "^$" (buffer-string) (point)))
        age
        aspell
        clisp
        entr
        fd
        fzf
        gcc
        gnumake
        htop
        jq
        nil
        nix-prefetch-scripts
        nixpkgs-fmt
        pandoc
        passage
        ripgrep
        rustup
        sbcl
        unzip
        utils-go
        utils-rust
        utils-scripts
      ];

      sessionVariables = {
        VISUAL = "$EDITOR";
        FCEDIT = "$EDITOR";
        CDPATH = ".:$HOME:$HOME/src";
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
