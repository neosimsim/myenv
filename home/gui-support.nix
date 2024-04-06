{ config, pkgs, lib, ... }:
{
  options.myenv = {
    guiSupport = lib.mkOption {
      type = lib.types.bool;
      default = with config.myenv; useWayland || managePlasma;
    };
  };

  config = lib.mkIf config.myenv.guiSupport {
    home = {
      sessionVariables = {
        EDITOR = "emacsclient -a ''";
        BROWSER = "chromium";
      };
      file = {
        "lib/plumbing".source = ./plumbing;
      };
      packages = with pkgs; [
        acmego
        editinacme
        meld
        Watch
      ];
    };

    xdg.configFile."alacritty/alacritty.yml".text = ''
      env:
        TERM: xterm-256color

      cursor:
        style:
          shape: Beam

      font:
        normal:
          family: DejaVuSans Mono
        size: 12.0

      # adapted from https://github.com/rajasegar/alacritty-themes/blob/2caad0a3598137a12fb4583298d715e5971fb134/themes/3024.light.yml
      colors:
        name: 3024 (light)
        author: Chris Kempson
        primary:
          background: "#f7f7f7"
          foreground: "#4a4543"
        cursor:
          text: "#f7f7f7"
          cursor: "#4a4543"
        normal:
          black: "#090300"
          red: "#db2d20"
          green: "#01a252"
          yellow: "#e8ba04"
          blue: "#01a0e4"
          magenta: "#a16a94"
          cyan: "#7bb4c6"
          white: "#a5a2a2"
        bright:
          black: "#5c5855"
          red: "#db2d20"
          green: "#01a252"
          yellow: "#fded02"
          blue: "#01a0e4"
          magenta: "#a16a94"
          cyan: "#b5e4f4"
          white: "#f7f7f7"
    '';
  };
}
