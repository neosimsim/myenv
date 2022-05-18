{ pkgs, config, lib, ... }: with lib;
let

  dotfiles = files: genAttrs files (name: {
    source = ../dotfiles + "/${name}";
    target = ".${name}";
  });

  configFiles = files: genAttrs files (name: {
    source = ../dotfiles + "/${name}";
  });

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
      home = {
        stateVersion = "22.05";

        packages = [
          (with config.myenv; import ./packages.nix { inherit pkgs enableGui useXServer useSway; })
        ];

        file =
          {
            ".profile".text = ''
              . "${../dotfiles/profile}"
              . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
            '';
          }
          // dotfiles [
            "cabal/config"
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

      programs.fish = {
        enable = true;
        shellInit = ''
          source "${../dotfiles/fish/config.fish}"
        '';
      };
    }

    (lib.mkIf config.myenv.enableGui {
      home.file = {
        "lib/plumbing".source = ../dotfiles/plumbing;
      };


      xdg.configFile = configFiles [
        "alacritty/alacritty.yml"
      ];

      programs = {
        firefox = {
          enable = true;

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

        vscode = {
          enable = true;

          package = pkgs.vscodium;

          extensions =
            let
              marketplaceExtensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
                {
                  name = "rust";
                  publisher = "rust-lang";
                  version = "0.7.8";
                  sha256 = "sha256-Y33agSNMVmaVCQdYd5mzwjiK5JTZTtzTkmSGTQrSNg0=";
                }
              ];
            in
            with pkgs.vscode-extensions; [
              haskell.haskell
              justusadam.language-haskell
              jnoortheen.nix-ide
              elixir-lsp.vscode-elixir-ls
              dbaeumer.vscode-eslint
            ] ++ marketplaceExtensions;

          userSettings = {
            "workbench.colorTheme" = "Default Light+";
            "nix.enableLanguageServer" = true;
            "terminal.integrated.env.linux" = {
              "EDITOR" = "codium -w";
            };
          };
        };
      };
    })

    (lib.mkIf config.myenv.useXServer {
      home.file = dotfiles [
        "xinitrc"
        "xmobarrc"
        "Xmodmap"
        "Xresources"
        "xsession"
      ];

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
          output = {
            eDP-1 = {
              scale = "1";
              resolution = "1920x1080";
            };
          };
          input = {
            "*" = {
              xkb_layout = "us";
              xkb_variant = "colemak";
            };
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
        packages = [
          # waybar icons
          pkgs.font-awesome
        ];

        sessionVariables = {
          MOZ_ENABLE_WAYLAND = 1;
        };
      };

    })
  ]);

}
