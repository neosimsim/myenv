{ config, pkgs, lib, ... }:
{
  options.myenv = {
    guiSupport = lib.mkOption {
      type = lib.types.bool;
      default = with config.myenv; manageWayland || managePlasma;
    };
  };

  config = lib.mkIf config.myenv.guiSupport {
    home = {
      sessionVariables = {
        EDITOR = "emacsclient -a ''";
        BROWSER = "chromium";
      };
    };
  };
}
