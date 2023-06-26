# To find out configurations use:
# nix run github:pjones/plasma-manager
{ config, lib, ... }: with lib;
{
  options = {
    myenv.managePlasma5 = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.myenv.managePlasma5 {
    # Disable kwallet popups for chromium
    # https://superuser.com/questions/994551/disable-kwallet-popups-from-chrome
    home.sessionVariables.CHROMIUM_FLAGS = "--password-store=basic";

    programs.plasma = {
      enable = true;

      files = {
        "baloofilerc"."General"."only basic indexing" = true;
        "kwinrc"."Desktops"."Number" = 4;
      };

      shortcuts = {
        "kwin"."Window No Border" = "Meta+B";
        "kwin"."Switch to Next Desktop" = "Meta+Ctrl+Right";
        "kwin"."Switch to Previous Desktop" = "Meta+Ctrl+Left";
      };
    };
  };
}
