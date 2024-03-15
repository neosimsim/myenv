{ pkgs, config, lib, ... }: with lib;
{
  options.myenv = {
    manageSway = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.myenv.manageSway {
    myenv.useWayland = true;

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
        mako
        wofi
        pulseaudio
      ];

      sessionVariables = {
        MOZ_ENABLE_WAYLAND = 1;
        BROWSER = "chromium";
      };
    };

  };
}
