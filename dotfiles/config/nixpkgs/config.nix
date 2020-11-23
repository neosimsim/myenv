{
  packageOverrides = pkgs: with pkgs; rec {
    myProfile = writeText "my-profile" ''
      export PATH=$HOME/.nix-profile/bin:$PATH
      export MANPATH=$HOME/.nix-profile/share/man:$MANPATH
    '';
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        (runCommand "profile" {} ''
          mkdir -p $out/etc/profile.d
          cp ${myProfile} $out/etc/profile.d/my-profile.sh
        '')
        jq
        haskellPackages.pandoc
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc"];
      extraOutputsToInstall = [ "man" "doc" ];
    };
    myGuiPackages = pkgs.buildEnv {
      name = "my-gui-packages";
      paths = [
        myPackages
        signal-desktop
        wire-desktop
        haskellPackages.xmonad
        xmobar
        dmenu
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc"];
      extraOutputsToInstall = [ "man" "doc" ];
    };

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
  };
}

