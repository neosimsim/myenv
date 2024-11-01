{ pkgs, lib, config, ... }:
lib.mkIf config.myenv.coreutils.enable {
  home.packages = [ pkgs.tmux ];
  home.file.".tmux.conf".source = ./tmux.conf;
}
