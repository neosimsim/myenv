{ pkgs, config, lib, ... }: with lib;
let
  aspell = pkgs.aspellWithDicts (p: with p; [ en de ]);
in
{
  options.myenv = {

    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableGuiTools = mkOption {
      type = types.bool;
      default = with config.myenv; manageSway || managePlasma;
    };
  };

  imports = [
    ./emacs
    ./git
    ./plasma.nix
    ./sway.nix
    ./chromium.nix
    ./firefox.nix
    ./ghc.nix
  ];

  config = mkIf config.myenv.enable (lib.mkMerge [
    {
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
          nixpkgs-fmt
          nix-prefetch-scripts
          passage
          plan9port
          ripgrep
          rnix-lsp
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
        } // (lib.optionalAttrs (! config.myenv.enableGuiTools) {
          EDITOR = "emacsclient -ca  ''";
        });

        # don't use sessionPath because I want to prefix PATH
        sessionVariablesExtra = ''
          PATH=$HOME/bin:$PATH
          PATH=$HOME/bin/aliases:$PATH
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
    }

    (lib.mkIf pkgs.stdenv.isDarwin {
      targets.darwin.search = "DuckDuckGo";

      # Link manually until Home Manager enables it again
      # https://github.com/nix-community/home-manager/blob/db00b39a9abec04245486a01b236b8d9734c9ad0/modules/targets/darwin/linkapps.nix
      # https://github.com/nix-community/home-manager/issues/1341#issuecomment-687286866
      home.file."Applications/Home Manager Apps".source =
        let
          apps = pkgs.buildEnv {
            name = "home-manager-applications";
            paths = config.home.packages;
            pathsToLink = "/Applications";
          };
        in
        "${apps}/Applications";
    })

    (lib.mkIf config.myenv.enableGuiTools {
      home = {
        sessionVariables = {
          EDITOR = "emacsclient -a ''";
          BROWSER = "chromium";
        };
        file = {
          "lib/plumbing".source = ./plumbing;
        };
        packages = with pkgs; [
          acmego
          editinacme
          meld
          Watch
        ] ++ lib.optionals pkgs.stdenv.isLinux [
          alacritty
          klavaro
          mplayer
          signal-desktop
          superTux
          xsel
          zathura
        ];
      };

      xdg.configFile."alacritty/alacritty.yml".text = ''
        env:
          TERM: xterm-256color

        cursor:
          style:
            shape: Beam

        font:
          normal:
            family: DejaVuSans Mono
          size: 12.0

        # adapted from https://github.com/rajasegar/alacritty-themes/blob/2caad0a3598137a12fb4583298d715e5971fb134/themes/3024.light.yml
        colors:
          name: 3024 (light)
          author: Chris Kempson
          primary:
            background: "#f7f7f7"
            foreground: "#4a4543"
          cursor:
            text: "#f7f7f7"
            cursor: "#4a4543"
          normal:
            black: "#090300"
            red: "#db2d20"
            green: "#01a252"
            yellow: "#e8ba04"
            blue: "#01a0e4"
            magenta: "#a16a94"
            cyan: "#7bb4c6"
            white: "#a5a2a2"
          bright:
            black: "#5c5855"
            red: "#db2d20"
            green: "#01a252"
            yellow: "#fded02"
            blue: "#01a0e4"
            magenta: "#a16a94"
            cyan: "#b5e4f4"
            white: "#f7f7f7"
      '';
    })

    (lib.mkIf (pkgs.stdenv.isLinux && config.myenv.enableGuiTools) {

      home.sessionVariables = {
        XDG_DESKTOP_DIR = "$HOME";
        XDG_DOCUMENTS_DIR = "$HOME/doc";
        XDG_DOWNLOAD_DIR = "$HOME/Downloads";
        XDG_MUSIC_DIR = "$HOME/music";
        XDG_PICTURES_DIR = "$HOME/pictures";
        XDG_PUBLICSHARE_DIR = "$HOME/Public";
        XDG_TEMPLATES_DIR = "$HOME/Templates";
        XDG_VIDEOS_DIR = "$HOME/Videos";
      };

      xresources.properties = lib.optionalAttrs (! config.myenv.manageSway) {
        "Xft.autohint" = 0;
        "Xft.lcdfilter" = "lcddefault";
        "Xft.hintstyle" = "hintslight";
        "Xft.hinting" = 1;
        "Xft.antialias" = 1;
        "Xft.rgba" = "rgb";

        "URxvt.font" = "xft=DejaVuSans Mono=size=14";
        "URxvt.scrollBar" = "False";
        "URxvt.perl-ext-common" = "default,font-size,color-themes";

        "URxvt.keysym.C-plus" = "font-size=increase";
        "URxvt.keysym.C-minus" = "font-size=decrease";
        "URxvt.keysym.C-equal" = "font-size=reset";
        "URxvt.keysym.C-slash" = "font-size=show";

        "URxvt.keysym.M-C-n" = "perl=color-themes=next";
        "URxvt.keysym.M-C-p" = "perl=color-themes=prev";
        "URxvt.color-themes.autosave" = 1;
        "URxvt.keysym.M-C-l" = "perl=color-themes=load-state";
        "URxvt.keysym.M-C-s" = "perl=color-themes=save-state";
      };

    })
  ]);
}
