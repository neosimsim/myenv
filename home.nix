{ pkgs, config, lib, ... }: with lib; {
  options.myenv = {
    enableGui = mkOption {
      type = types.bool;
      default = false;
    };
  };
  config = {
    home.packages =
      let
        myenv = with config.myenv; import ./. { inherit pkgs enableGui; };
      in
      [ myenv ];

    home.file =
      let
        dotfilesCore = [
          "cabal/config"
          "ghci"
          "gnupg/gpg-agent.conf"
          "mbsyncrc"
          "mkshrc"
          "mutt/abnrc"
          "mutt/mailcap"
          "mutt/muttrc"
          "mutt/posteorc"
          "profile"
          "tmux.conf"
        ];
        dotfilesGui = [
          "themes/urxvt/atom-one-light"
          "themes/urxvt/tango"
          "xinitrc"
          "xmobarrc"
          "Xmodmap"
          "Xmodmap.colemak"
          "Xmodmap.qwerty"
          "xmonad/build"
          "xmonad/xmonad.hs"
          "Xresources"
          "xsession"
        ];
        dotfiles = dotfilesCore ++ optionals config.myenv.enableGui dotfilesGui;
      in
      genAttrs dotfiles
        (name:
          {
            source = ./dotfiles + "/${name}";
            target = ".${name}";
          });

    xdg.configFile =
      let
        configFilesCore = [
          "emacs/init.el"
          "git/config"
          "git/ignore"
          "vis/visrc.lua"
          "vis/themes/peaksea.lua"
        ];
        configFilesGui = [
          "alacritty/alacritty.yml"
        ];
        configFiles = configFilesCore ++ optionals config.myenv.enableGui configFilesGui;
      in
      genAttrs configFiles (name: { source = ./dotfiles + "/${name}"; });
  };
}
