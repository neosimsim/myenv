{ pkgs, config, lib, ... }: with lib;
let

  dotfiles = files: genAttrs files (name: {
    source = ../dotfiles + "/${name}";
    target = ".${name}";
  });

  configFiles = files: genAttrs files (name: {
    source = ../dotfiles + "/${name}";
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

    useSway = mkOption {
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


  config = mkIf config.myenv.enable (lib.mkMerge [
    {
      nix = {
        package = pkgs.nix;
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
        ]) ++ (with haskellPackages; map haskell.lib.justStaticExecutables [
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
          EDITOR = "emacsclient -ca  ''";
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
            then emacsPgtk
            else
              if config.myenv.enableGui
              then emacsGit
              else emacsGit-nox;

          extraConfig = builtins.readFile ../dotfiles/emacs/init.el;
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
            elm-mode
            elixir-mode
            rust-mode
            elisp-format

            lsp-mode
            lsp-ui
            flycheck
            company
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
    })

    (lib.mkIf config.myenv.enableGui {
      home = {
        file = {
          "lib/plumbing".source = ../dotfiles/plumbing;
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

          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            ublock-origin
            vimium
          ];

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
        file = dotfiles [
          "Xmodmap"
        ];

        sessionVariables = {
          EDITOR = "emacsclient -a ''";
          BROWSER = "chromium";
        };

        packages = with pkgs; [
          brightnessctl
          dmenu
          feh
          numlockx
          rxvt-unicode
          scrot
          signal-desktop
          sxiv
          xsel
        ] ++ (with pkgs.xorg; [
          xkill
          xmodmap
        ]) ++ (with pkgs.xfce; [
          thunar
        ]) ++ (with pkgs.haskellPackages; [
          xmobar
        ]);
      };

      xsession = {
        enable = true;
        initExtra = ''
          xautolock -cornerdelay 1 -cornerredelay 5 -time 1 -locker 'slock' -corners 0-00 &

          # TODO observe strange behaviour with modifier
          # rotatekb colemak
          numlockx
          [ -f ~/.fehbg ] && sh ~/.fehbg
          amixer set Master mute
          amixer -c 0 set Headphone unmute
          amixer -c 0 set Headphone 70

          # plan9port
          if which 9 >/dev/null; then
            export NAMESPACE=$HOME/9p
            # export EDITOR="9 editinacme"
            export tabstop=4
            # font for sam, 9term (not acme)
            export font=/mnt/font/DejaVuSansMono/18a/font
            # a &
            9 plumber
          fi

          ec -n

          signal-desktop &
        '';
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

      xdg.configFile =
        configFiles [
          "xmobar/xmobar.hs"
        ] // {
          "xmobar/xmobar".source =
            let
              ghc = pkgs.haskellPackages.ghcWithPackages (p: with p; [ xmobar ]);

            in
            pkgs.runCommand "xmobar-compile"
              {
                buildInputs = [ ghc ];
                ghcFlags = "--make -i -ilib -fforce-recomp -main-is main -v0 -threaded -rtsopts -with-rtsopts -V0";
              } ''
              # compiles flags copied from
              # https://hackage.haskell.org/package/xmobar-0.43/docs/src/Xmobar.App.Compile.html#recompile
              ghc -o $out ${config.xdg.configFile."xmobar/xmobar.hs".source} $ghcFlags
            '';
        };

      xsession.windowManager.xmonad = {
        enable = true;
        config = ../dotfiles/xmonad/xmonad.hs;
        enableContribAndExtras = true;
      };
    })

    (lib.mkIf config.myenv.useSway {
      wayland.windowManager.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        config = {
          modifier = "Mod4";
          fonts = {
            names = [ "Liberation Sans" "Noto Color Emoji" ];
            size = 12.0;
          };
          terminal = "alacritty";
          menu = "wofi --show run";
          bars = [{
            command = "waybar";
          }];
          input = {
            "*" = {
              xkb_layout = "us";
              xkb_variant = "colemak";
              pointer_accel = "-0.8";
            };
            "type:touchpad" = {
              pointer_accel = "0";
              tap = "disabled";
            };
          };
          keybindings =
            let
              modifier = config.wayland.windowManager.sway.config.modifier;
            in
            lib.mkOptionDefault {
              "${modifier}+Control+Shift+left" = "move workspace to output left";
              "${modifier}+Control+Shift+right" = "move workspace to output right";
            };
        };
        extraConfig = ''
          # Brightness
          bindsym XF86MonBrightnessDown exec "brightnessctl set 5%-"
          bindsym XF86MonBrightnessUp exec "brightnessctl set +5%"

          # Volume
          bindsym XF86AudioRaiseVolume exec 'pactl set-sink-volume @DEFAULT_SINK@ +1%'
          bindsym XF86AudioLowerVolume exec 'pactl set-sink-volume @DEFAULT_SINK@ -1%'
          bindsym XF86AudioMute exec 'pactl set-sink-mute @DEFAULT_SINK@ toggle'
        '';
      };

      programs = {
        waybar.enable = true;

        fish.loginShellInit = ''
          if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]
            exec sway
          end
        '';
      };

      home = {
        packages = with pkgs; [
          sway-contrib.grimshot
          # waybar icons
          font-awesome
          brightnessctl
          swaylock
          swayidle
          wl-clipboard
          mako
          wofi
          pulseaudio
        ];

        sessionVariables = {
          MOZ_ENABLE_WAYLAND = 1;
          BROWSER = "chromium";
        };
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
