{ pkgs, lib, config, ... }:
lib.mkIf (pkgs.stdenv.isLinux && config.myenv.coreutils.enable) {
  home.packages = with pkgs; [
    racket-minimal
  ];
}
