{
  packageOverrides = pkgs: with pkgs;
    let
      # Using the ALSA plugin of xmobar e.g. by adding
      #
      #   'Run Alsa "default" "Master" []'
      #
      # to $HOME/.xmobarrc does not work reporting the
      #
      #   ALSA lib dlmisc.c:338:(snd_dlobj_cache_get0) Cannot open shared library
      #     libasound_module_ctl_pulse.so (libasound_module_ctl_pulse.so:
      #     libasound_module_ctl_pulse.so: cannot open shared object file: No such
      #     file or directory)
      #
      # see https://github.com/NixOS/nixpkgs/issues/6860.
      #
      # As a workaround for this linking issues create a wrapper script
      # adding alsa-plugin to LD_LIBRARY_PATH before running xmobar.
      xmobar =
        let myXmobar = haskellPackages.xmobar.overrideAttrs (oldAttrs: rec {
                         configureflags = [
                           "-f with_utf8"
                           "-f with_xft"
                           "-f with_alsa"
                           "-f with_inotify"
                           "-f -with_weather"
                         ];
                       });
        in writeShellScriptBin "xmobar" ''
              export LD_LIBRARY_PATH=${alsaPlugins}/lib/alsa-lib
              exec ${myXmobar}/bin/xmobar
           '';
    in {
      myPackages = pkgs.buildEnv {
        name = "my-packages";
        paths = [
          # make sure nix-shell runs mksh
          (writeShellScriptBin "nix-shell" ''
            exec ${nix}/bin/nix-shell --run ${mksh}/bin/mksh "$@"
          '')

          agda
          beamPackages.elixir
          cargo
          fzf
          gnupg
          isync
          jq
          mutt
          pinentry-curses
          plan9port
          rustc
          vis
        ] ++ (with haskellPackages; [
          (ghc.withPackages (p: with p; [
            containers
            extra
            filepath
            hspec
            QuickCheck
          ]))
          apply-refact
          cabal2nix
          cabal-fmt
          cabal-install
          hlint
          ormolu
          pandoc
          steeloverseer
        ]);
        pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc"];
        extraOutputsToInstall = [ "man" "doc" ];
      };
      myGuiPackages = pkgs.buildEnv {
        name = "my-gui-packages";
        paths = [
          alacritty
          dmenu
          feh
          haskellPackages.threadscope
          haskellPackages.xmonad
          klavaro
          signal-desktop
          wire-desktop
          xmobar
          xorg.xmodmap
        ];
        pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc"];
        extraOutputsToInstall = [ "man" "doc" ];
      };
    };
}

