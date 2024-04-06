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
        tmux
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

      file.".tmux.conf".text = ''
        set -g base-index 1
        set -g pane-base-index 1
        set -g mouse off
        set -g status-position top
        set -g status-interval 1
        set -g escape-time 0
        set -g default-terminal "tmux-256color"

        set -g word-separator ""

        bind-key -T copy-mode    C-o                 if-shell -F "#{?selection_present,0,1}" "send-keys -X select-word" \; run-shell 'tmux-run-selection-with dtt'

        bind-key -T copy-mode-vi    P                 send-keys -X copy-selection-and-cancel \; paste-buffer
        # pass the word under cursor to `dtt`
        bind-key -T copy-mode-vi    p                 if-shell -F "#{?selection_present,0,1}" "send-keys -X select-word" \; run-shell 'tmux-run-selection-with dtt'
        bind-key -T copy-mode-vi    C-Space           send-keys -X select-word

        # don't copy selection on MouseDragEnd1Pane, we want to be more flexible
        unbind-key -T copy-mode-vi  MouseDragEnd1Pane
        bind-key -T root            DoubleClick1Pane  copy-mode -M \; send-keys -X select-word

        bind-key -T copy-mode-vi    MouseUp2Pane      send-keys -X copy-selection-and-cancel

        # drag right mouse button to pass selection to dtt(1)
        unbind-key -T root          MouseDown3Pane
        bind-key -T root            MouseDrag3Pane    if-shell -Ft = "#{mouse_any_flag}" "if -Ft= \"#{pane_in_mode}\" \"copy-mode -M\" \"send-keys -M\"" "copy-mode -M"
        bind-key -T copy-mode-vi    MouseDrag3Pane    send-keys -X begin-selection
        bind-key -T copy-mode-vi    MouseDragEnd3Pane send-keys -X copy-pipe-and-cancel "xargs tmux split-window -c #{pane_current_path} dtt"

        # click right mouse button to pass word under cursos to dtt(1)
        bind-key -T root            MouseUp3Pane      copy-mode -M \; send-keys -X select-word \; run-shell 'tmux-run-selection-with dtt'

        bind-key -T root            MouseDown3Status  choose-tree -Zw
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
