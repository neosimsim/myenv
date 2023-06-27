{ pkgs, config, lib, ... }: with lib;
let

  dotfiles = files: genAttrs files (name: {
    source = ../../dotfiles + "/${name}";
    target = ".${name}";
  });

  configFiles = files: genAttrs files (name: {
    source = ../../dotfiles + "/${name}";
  });

  aspell = pkgs.aspellWithDicts (p: with p; [ en de ]);

  git = pkgs.git.override {
    guiSupport = config.myenv.enableGuiTools;
  };

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
  options.myenv = {

    enable = mkOption {
      type = types.bool;
      default = false;
    };

    manageXsession = mkOption {
      type = types.bool;
      default = false;
    };

    enableGuiTools = mkOption {
      type = types.bool;
      default = with config.myenv; manageXsession || manageSway || managePlasma5;
    };
  };

  imports = [
    ./xmonad.nix
    ./plasma.nix
    ./sway.nix
    ./chromium.nix
    ./firefox.nix
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
          cabalShell
          dhall
          dhall-json
          entr
          fd
          fzf
          gcc
          ghc
          git
          git-lfs
          gnumake
          go
          goScripts
          gosec
          gotools
          haskellScripts
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
          scripts
          texlive-full
          tmux
          typespeed
          unzip
        ] ++ (with elmPackages; [
          elm
          elm-format
          elm-language-server
          elm-review
          elm-test
        ]) ++ (with haskellPackages; [
          apply-refact
          cabal2nix
          cabal-fmt
          cabal-install
          hconv
          hindent
          hlint
          hookmark
          ormolu
          pandoc
          stylish-haskell
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

        file = dotfiles [
          "ghci"
          "mkshrc"
          "tmux.conf"
        ];
      };

      xdg.configFile = configFiles [
        "git/attributes"
        "git/config"
        "git/ignore"
      ];

      programs = {
        fish = {
          enable = true;
          shellInit = ''
            set -U fish_greeting
          '';
        };

        emacs = {
          enable = true;
          package = with pkgs;
            if config.myenv.manageSway
            # use emacs Pure GTK to make use of Wayland scaling
            then emacs-pgtk
            else
              if config.myenv.enableGuiTools
              then emacs-git
              else emacs-git-nox;

          extraConfig = builtins.readFile ../../dotfiles/emacs/init.el;
          extraPackages = epkgs: with epkgs; [
            spacemacs-theme
            kaolin-themes
            ivy
            counsel
            amx
            highlight-symbol
            magit
            htmlize
            mixed-pitch
            avy
            buffer-move
            osm
            vimgolf

            haskell-mode
            nix-mode
            fish-mode
            dhall-mode
            elm-mode
            erlang
            elixir-mode
            rust-mode
            elisp-format

            lsp-mode
            lsp-ui
            flycheck
            company
            yasnippet
            lsp-haskell
          ];
        };
      };
    }

    (lib.mkIf pkgs.stdenv.isLinux {
      home.packages = with pkgs; [
        haskell-language-server
      ];
    })

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
          "lib/plumbing".source = ../../dotfiles/plumbing;
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
          superTux
          zathura
        ];
      };

      xdg.configFile = configFiles [
        "alacritty/alacritty.yml"
      ];
    })

    (lib.mkIf config.myenv.manageXsession {
      home.packages = with pkgs; [
        signal-desktop
        sxiv
        xsel
      ];

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

      xresources.properties = {
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

    (lib.mkIf pkgs.stdenv.isOpenBSD {
      # workaround for haskell, due to w^x on OpenBSD
      home.shellAliases = {
        cabal = "env TMPDIR=/usr/local/cabal/build/ cabal";
      };
    })

  ]);
}