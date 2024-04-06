{ pkgs, lib, config, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = with pkgs; lib.optionals config.myenv.guiSupport [
    alacritty
    klavaro
    mplayer
    signal-desktop
    superTux
    xsel
    zathura
  ];

  home.sessionVariables = lib.optionalAttrs config.myenv.guiSupport {
    XDG_DESKTOP_DIR = "$HOME";
    XDG_DOCUMENTS_DIR = "$HOME/doc";
    XDG_DOWNLOAD_DIR = "$HOME/Downloads";
    XDG_MUSIC_DIR = "$HOME/music";
    XDG_PICTURES_DIR = "$HOME/pictures";
    XDG_PUBLICSHARE_DIR = "$HOME/Public";
    XDG_TEMPLATES_DIR = "$HOME/Templates";
    XDG_VIDEOS_DIR = "$HOME/Videos";
  };

  xresources.properties = lib.optionalAttrs (config.myenv.guiSupport && !config.myenv.useWayland) {
    "Xft.autohint" = 0;
    "Xft.lcdfilter" = "lcddefault";
    "Xft.hintstyle" = "hintslight";
    "Xft.hinting" = 1;
    "Xft.antialias" = 1;
    "Xft.rgba" = "rgb";

    "URxvt.font" = "xft=DejaVuSans Mono=size=14";
    "URxvt.scrollBar" = "False";
    "URxvt.perl-ext-common" = "default,font-size,color-themes";

    "URxvt.keysym.C-plus" = "font-size=increase";
    "URxvt.keysym.C-minus" = "font-size=decrease";
    "URxvt.keysym.C-equal" = "font-size=reset";
    "URxvt.keysym.C-slash" = "font-size=show";

    "URxvt.keysym.M-C-n" = "perl=color-themes=next";
    "URxvt.keysym.M-C-p" = "perl=color-themes=prev";
    "URxvt.color-themes.autosave" = 1;
    "URxvt.keysym.M-C-l" = "perl=color-themes=load-state";
    "URxvt.keysym.M-C-s" = "perl=color-themes=save-state";
  };

}
