{ config, pkgs, lib, ... }: {
  options = {
    myenv.plan9port = {
      enable = lib.mkEnableOption ''
        Install and configure plan9port
      '';
    };
  };

  config = lib.mkIf config.myenv.plan9port.enable {
    home.file."lib/plumbing".source = ./plumbing;

    home.packages = with pkgs; [
      plan9port
      acmego
      editinacme
      Watch
    ];
  };
}
