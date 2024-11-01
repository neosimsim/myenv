{ config, lib, pkgs, ... }: {
  options = {
    myenv.go = {
      enable = lib.mkEnableOption ''
        Enable go development environment
      '';
    };
  };

  config = lib.mkIf config.myenv.go.enable {
    home.packages = with pkgs; [
      go
      gosec
      gotools
    ];

    home.sessionVariables = {
      GOOS = lib.mkDefault "linux";
      GOARCH = lib.mkDefault "amd64";
      GOBIN = "$HOME/bin";
    };
  };
}