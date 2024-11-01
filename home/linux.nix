{ pkgs, lib, config, ... }:
lib.mkIf (pkgs.stdenv.isLinux && config.myenv.coreutils.enable) {
  home.sessionVariables = {
    XDG_DESKTOP_DIR = "$HOME";
    XDG_DOCUMENTS_DIR = "$HOME/doc";
    XDG_DOWNLOAD_DIR = "$HOME/Downloads";
    XDG_MUSIC_DIR = "$HOME/music";
    XDG_PICTURES_DIR = "$HOME/pictures";
    XDG_PUBLICSHARE_DIR = "$HOME/Public";
    XDG_TEMPLATES_DIR = "$HOME/Templates";
    XDG_VIDEOS_DIR = "$HOME/Videos";
  };
}
