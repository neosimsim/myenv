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
      gopls
      gosec
      gotools # e.g. goimports
      go-tools # e.g. staticcheck
    ];

    home.sessionVariables = {
      GOOS = lib.mkDefault "linux";
      GOARCH = lib.mkDefault "amd64";
      GOBIN = "$HOME/bin";
    };
  };
}
