{ pkgs, config, lib, ... }: with lib; {
  options.myenv = {

    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableGui = mkOption {
      type = types.bool;
      default = false;
    };
  };


  config = mkIf config.myenv.enable {
    home.packages =
      let
        myenv = with config.myenv; import ./. { inherit pkgs enableGui; };
      in
      [ (lowPrio myenv) ];

    home.file =
      let
        dotfilesCore = [
          "cabal/config"
          "ghci"
          "gnupg/gpg-agent.conf"
          "mkshrc"
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
          })
      // optionalAttrs config.myenv.enableGui {
        "lib/plumbing".source = ./dotfiles/plumbing;
      };

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

    programs.vscode = {
      enable = true;

      package = pkgs.vscodium;

      extensions =
        let
          marketplaceExtensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "rust";
              publisher = "rust-lang";
              version = "0.7.8";
              sha256 = "sha256-Y33agSNMVmaVCQdYd5mzwjiK5JTZTtzTkmSGTQrSNg0=";
            }
          ];
        in
        with pkgs.vscode-extensions; [
          haskell.haskell
          justusadam.language-haskell
          jnoortheen.nix-ide
        ] ++ marketplaceExtensions;

      userSettings = {
        "workbench.colorTheme" = "Default Light+";
        "nix.enableLanguageServer" = true;
      };

    };
  };
}
