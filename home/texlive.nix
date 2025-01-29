{ config, lib, pkgs, ... }: {
  options = {
    myenv.texlive = {
      enable = lib.mkEnableOption ''
        Enable texlive environment
      '';
    };
  };

  config = lib.mkIf config.myenv.texlive.enable {
    home.packages = with pkgs; [
      texlive-full
    ];
  };
}
