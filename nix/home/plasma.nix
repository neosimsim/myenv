# To find out configurations use:
# nix run github:pjones/plasma-manager
{ config, lib, pkgs, ... }: with lib;
{
  options = {
    myenv.managePlasma = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.myenv.managePlasma {
    home.sessionVariables = {
      PLASMA_USE_QT_SCALING = 1;
    };

    home.packages = with pkgs; [
      kalendar
      kompare
    ];

    programs.git.extraConfig = {
      diff.tool = "kompare";
      merge.tool = "kompare";
    };

    programs.plasma = {
      enable = true;

      files = {
        "baloofilerc"."General"."only basic indexing" = true;
        "kwalletrc"."Wallet"."Enabled" = true;
        "kwinrc"."Desktops"."Number" = 4;
        "kcminputrc"."Mouse"."XLbInptAccelProfileFlat" = true;
        "kcminputrc"."Mouse"."XLbInptPointerAcceleration" = "-0.4";
      };

      shortcuts = {
        "kwin"."Window No Border" = "Meta+B";
        "kwin"."Switch to Next Desktop" = "Meta+Ctrl+Right";
        "kwin"."Switch to Previous Desktop" = "Meta+Ctrl+Left";
      };
    };
  };
}
