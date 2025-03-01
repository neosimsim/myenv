{ pkgs, lib, config, ... }: {
  options = {
    myenv.xserver = {
      enable = lib.mkEnableOption ''
        Setup environment for X Server.
      '';
    };
  };

  config = lib.mkIf config.myenv.xserver.enable {
    myenv.emacs.package = pkgs.emacs-git;
    home.sessionVariables.EDITOR = "emacsclient -a ''";

    xresources.properties = {
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
  };
}
