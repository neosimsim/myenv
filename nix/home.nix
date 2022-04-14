{ pkgs, config, lib, ... }: with lib; {
  options.myenv = {

    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableGui = mkOption {
      type = types.bool;
      default = false;
    };
  };


  config = mkIf config.myenv.enable {
    home.packages = [
      (with config.myenv; import ./packages.nix { inherit pkgs enableGui; })
    ];

    home.stateVersion = "22.05";

    home.file =
      let
        dotfilesCore = [
          "cabal/config"
          "ghci"
          "mkshrc"
          "tmux.conf"
        ];
        dotfilesGui = [
          "xinitrc"
          "xmobarrc"
          "Xmodmap"
          "xmonad/build"
          "xmonad/xmonad.hs"
          "Xresources"
          "xsession"
        ];
        dotfiles = dotfilesCore ++ optionals config.myenv.enableGui dotfilesGui;
      in
      {
        ".profile".text = ''
          . "${../dotfiles/profile}"
          . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
        '';
      }
      // optionalAttrs config.myenv.enableGui {
        "lib/plumbing".source = ../dotfiles/plumbing;
      }
      // genAttrs dotfiles
        (name:
          {
            source = ../dotfiles + "/${name}";
            target = ".${name}";
          });

    xdg.configFile =
      let
        configFilesCore = [
          "git/attributes"
          "git/config"
          "git/ignore"
        ];
        configFilesGui = [
          "alacritty/alacritty.yml"
        ];
        configFiles = configFilesCore ++ optionals config.myenv.enableGui configFilesGui;
      in
      genAttrs configFiles (name: { source = ../dotfiles + "/${name}"; });

    programs = {

      firefox = {
        enable = true;

        package = pkgs.firefox-esr.override {
          extraPolicies = {
            SearchEngines = {
              Default = "DuckDuckGo";
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
  };
}
