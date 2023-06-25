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
    guiSupport = config.myenv.enableGui;
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

    useXServer = mkOption {
      type = types.bool;
      default = false;
    };

    enableGui = mkOption {
      type = types.bool;
      default = with config.myenv; useXServer || useSway;
    };
  };

  imports = [
    ./xmonad.nix
    ./sway.nix
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
        } // (lib.optionalAttrs (! config.myenv.useXServer) {
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
            if config.myenv.useSway
            # use emacs Pure GTK to make use of Wayland scaling
            then emacs-pgtk
            else
              if config.myenv.enableGui
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

    (lib.mkIf config.myenv.enableGui {
      home = {
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

      programs = {
        firefox = {
          enable = pkgs.stdenv.isLinux;

          package =
            let
              firefoxPkg =
                if config.myenv.useSway
                then pkgs.firefox-esr-wayland
                else pkgs.firefox-esr;
            in
            firefoxPkg.override {
              extraPolicies = {
                SearchEngines = {
                  Default = "DuckDuckGo";
                  Add = [
                    {
                      Name = "Hoogle";
                      URLTemplate = "https://hoogle.haskell.org/?hoogle={searchTerms}";
                      Alias = "hoogle";
                      IconURL = "https://hoogle.haskell.org/favicon.png";
                    }
                    {
                      Name = "Hackage";
                      URLTemplate = "https://hackage.haskell.org/packages/search?terms={searchTerms}";
                      Alias = "hackage";
                      IconURL = "https://hackage.haskell.org/static/favicon.png";
                    }
                  ];
                };
              };
            };

          # We need a profile because extensions listed here will only
          # be available in Firefox profiles managed by Home Manager.
          # https://github.com/nix-community/home-manager/blob/e39a9d0103e3b2e42059c986a8c633824b96c193/modules/programs/firefox.nix
          profiles = {
            default = {
              settings = {
                "signon.rememberSignons" = false;

                # disable all sorts of telemetry
                # https://support.mozilla.org/en-US/questions/1197144
                "browser.newtabpage.activity-stream.feeds.telemetry" = false;
                "browser.newtabpage.activity-stream.telemetry" = false;
                "browser.ping-centre.telemetry" = false;
                "datareporting.healthreport.service.enabled" = false;
                "datareporting.healthreport.uploadEnabled" = false;
                "datareporting.policy.dataSubmissionEnabled" = false;
                "datareporting.sessions.current.clean" = true;
                "devtools.onboarding.telemetry.logged" = false;
                "toolkit.telemetry.archive.enabled" = false;
                "toolkit.telemetry.bhrPing.enabled" = false;
                "toolkit.telemetry.enabled" = false;
                "toolkit.telemetry.firstShutdownPing.enabled" = false;
                "toolkit.telemetry.hybridContent.enabled" = false;
                "toolkit.telemetry.newProfilePing.enabled" = false;
                "toolkit.telemetry.prompted" = 2;
                "toolkit.telemetry.rejected" = true;
                "toolkit.telemetry.reportingpolicy.firstRun" = false;
                "toolkit.telemetry.server" = "";
                "toolkit.telemetry.shutdownPingSender.enabled" = false;
                "toolkit.telemetry.unified" = false;
                "toolkit.telemetry.unifiedIsOptIn" = false;
                "toolkit.telemetry.updatePing.enabled" = false;
              };

              extensions = with pkgs.nur.repos.rycee.firefox-addons; [
                ublock-origin
                vimium
              ];
            };
          };
        };

        chromium = rec {
          enable = pkgs.stdenv.isLinux;
          package = pkgs.ungoogled-chromium;
          # https://github.com/nix-community/home-manager/issues/2216
          extensions =
            let
              createChromiumExtensionFor = browserVersion: { id, sha256, version }:
                {
                  inherit id;
                  crxPath = builtins.fetchurl {
                    url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
                    name = "${id}.crx";
                    inherit sha256;
                  };
                  inherit version;
                };
              createChromiumExtension = createChromiumExtensionFor (lib.versions.major package.version);
            in
            [
              (createChromiumExtension {
                # https://chrome.google.com/webstore/detail/ublock-origin/cjpalhdlnbpafiamejdnhcphjbkeiagm
                id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
                version = "1.43.0";
                sha256 = "sha256:0izlm20b0bpjr98rw63c42zb04ky1mclg0da2xmj0kw3qichnpvg";
              })
              (createChromiumExtension {
                # https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb
                id = "dbepggeogbaibhgnhhndojpepiihcmeb";
                version = "1.67";
                sha256 = "sha256:097axwrhn8g26kp25w86x71khaqcw3nb0ras9ndwqvdw3bpgkcd8";
              })
            ];
        };
      };
    })

    (lib.mkIf config.myenv.useXServer {
      home = {
        sessionVariables = {
          EDITOR = "emacsclient -a ''";
          BROWSER = "chromium";
        };

        packages = with pkgs; [
          signal-desktop
          sxiv
          xsel
        ];
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
