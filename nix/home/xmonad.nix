{ pkgs, config, lib, ... }: with lib;
let

  dotfiles = files: genAttrs files (name: {
    source = ../../dotfiles + "/${name}";
    target = ".${name}";
  });

  configFiles = files: genAttrs files (name: {
    source = ../../dotfiles + "/${name}";
  });

in
{
  options = {
    myenv.useXmonad = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.myenv.useXmonad {
    xsession = {
      enable = true;
      initExtra = ''
        xautolock -cornerdelay 1 -cornerredelay 5 -time 1 -locker 'slock' -corners 0-00 &

        # TODO observe strange behaviour with modifier
        # rotatekb colemak
        numlockx
        [ -f ~/.fehbg ] && sh ~/.fehbg
        amixer set Master mute
        amixer -c 0 set Headphone unmute
        amixer -c 0 set Headphone 70

        # plan9port
        if which 9 >/dev/null; then
          export NAMESPACE=$HOME/9p
          # export EDITOR="9 editinacme"
          export tabstop=4
          # font for sam, 9term (not acme)
          export font=/mnt/font/DejaVuSansMono/18a/font
          # a &
          9 plumber
        fi

        ec -n

        signal-desktop &
      '';

      windowManager.xmonad = {
        enable = true;
        config = ../../dotfiles/xmonad/xmonad.hs;
        enableContribAndExtras = true;
      };
    };

    home = {

      file = dotfiles [
        "Xmodmap"
      ];

      sessionVariables = {
        EDITOR = "emacsclient -a ''";
        BROWSER = "chromium";
      };

      packages = with pkgs; [
        brightnessctl
        dmenu
        feh
        numlockx
        rxvt-unicode
      ] ++ (with pkgs.xorg; [
        xkill
        xmodmap
      ]) ++ (with pkgs.xfce; [
        thunar
      ]) ++ (with pkgs.haskellPackages; [
        xmobar
      ]);

    };

    xdg.configFile =
      configFiles [
        "xmobar/xmobar.hs"
      ] // {
        "xmobar/xmobar".source =
          let
            ghc = pkgs.haskellPackages.ghcWithPackages (p: with p; [ xmobar ]);

          in
          pkgs.runCommand "xmobar-compile"
            {
              buildInputs = [ ghc ];
              ghcFlags = "--make -i -ilib -fforce-recomp -main-is main -v0 -threaded -rtsopts -with-rtsopts -V0";
            } ''
            # compiles flags copied from
            # https://hackage.haskell.org/package/xmobar-0.43/docs/src/Xmobar.App.Compile.html#recompile
            ghc -o $out ${config.xdg.configFile."xmobar/xmobar.hs".source} $ghcFlags
          '';
      };

    gtk = {
      enable = true;

      iconTheme = {
        name = "breeze";
      };

      cursorTheme = {
        name = "breeze_cursors";
      };

      font = {
        name = "Noto Sans";
        size = 10;
      };

      gtk2.extraConfig = ''
        gtk-enable-animations=1
        gtk-primary-button-warps-slider=0
        gtk-toolbar-style=3
        gtk-menu-images=1
        gtk-button-images=1
        gtk-cursor-theme-size=24
      '';

      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = false;
        gtk-button-images = true;
        gtk-cursor-theme-size = 24;
        gtk-decoration-layout = "icon:minimize,maximize,close";
        gtk-enable-animations = true;
        gtk-menu-images = true;
        gtk-modules = "colorreload-gtk-module";
        gtk-primary-button-warps-slider = false;
        gtk-toolbar-style = 3;
      };

      gtk4.extraConfig = {
        gtk-application-prefer-dark-theme = false;
        gtk-cursor-theme-size = 24;
        gtk-decoration-layout = "icon:minimize,maximize,close";
        gtk-enable-animations = true;
        gtk-primary-button-warps-slider = false;
      };
    };
  };
}
