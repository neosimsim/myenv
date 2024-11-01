{ pkgs, lib, config, ... }: with lib;
{
  options = {
    myenv.firefox = {
      enable = mkEnableOption ''
        Install and configure Firefox.
      '';
    };
  };

  config = lib.mkIf config.myenv.firefox.enable {
    programs.firefox = {
      enable = true;

      package = pkgs.firefox-esr.override {
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
  };
}
