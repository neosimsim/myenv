{ config, lib, pkgs, ... }: with lib;
{
  options = {
    myenv.plasma = {
      enable = mkEnableOption ''
        Confige KDE plasma.
      '';
    };
  };

  config = mkIf config.myenv.plasma.enable {
    home.sessionVariables = {
      PLASMA_USE_QT_SCALING = 1;
    };

    home.packages = with pkgs.kdePackages; [
      kompare
      kigo
    ];

    programs.git.extraConfig = {
      diff.tool = "kompare";
      merge.tool = "kompare";
    };

    # To find out configurations use:
    # nix run github:pjones/plasma-manager
    programs.plasma = {
      enable = true;

      configFile = {
        "baloofilerc"."General"."only basic indexing" = true;
        "kwalletrc"."Wallet"."Enabled" = true;
        "kwinrc"."Desktops"."Number" = 1;

        "kcminputrc"."Keyboard"."NumLock" = 0;
        "kcminputrc"."Mouse"."XLbInptAccelProfileFlat" = true;
        "kcminputrc"."Mouse"."XLbInptPointerAcceleration" = "-0.4";
        "kcminputrc"."Libinput/12538/4416/INSTANT USB GAMING MOUSE "."PointerAcceleration" = "-0.200";
        "kcminputrc"."Libinput/12538/4416/INSTANT USB GAMING MOUSE "."PointerAccelerationProfile" = 1;

        "klaunchrc"."BusyCursorSettings"."Bouncing" = false;
        "klaunchrc"."FeedbackStyle"."BusyCursor" = false;
      };

      shortcuts = {
        "kwin"."Window No Border" = "Meta+B";
      };
    };

    programs.konsole = {
      enable = true;

      defaultProfile = "Custom";

      profiles.Custom = {
        colorScheme = "Breeze Light";
      };
    };
  };
}
