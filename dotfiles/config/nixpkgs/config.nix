{
  packageOverrides = pkgs: with pkgs; rec {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        jq
        fzf
        plan9port
        haskellPackages.cabal2nix
        haskellPackages.pandoc
        haskellPackages.steeloverseer
        hs
        agda
        vis
        emacs-nox
        gnupg
        pinentry-curses
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc"];
      extraOutputsToInstall = [ "man" "doc" ];
    };
    myGuiPackages = pkgs.buildEnv {
      name = "my-gui-packages";
      paths = [
        signal-desktop
        wire-desktop
        haskellPackages.xmonad
        haskellPackages.threadscope
        xmobar
        dmenu
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc"];
      extraOutputsToInstall = [ "man" "doc" ];
    };

    # I dont want to expose GHC etc. globally do avoid strange behaviours
    # with projects using nix-shell.
    #
    # On the other hand I want to install GHC etc. so I can enter a shell
    # quickly without having nix to fetch everything after running
    # `nix-collect-garbage`.
    #
    # As a solutions I hide GHC etc. behind a wrapper script `hs`
    # which will add the GHC etc. to PATH.
    #
    # Usage:
    #   hs ghc --version
    #
    # or to expose GHC to the current shell session
    #
    #   . hs
    hs =
      let myHaskellEnv = pkgs.buildEnv {
            name = "my-haskell-env";
            paths = with haskellPackages; [
              ghc
              cabal-install
              cabal-fmt
              haskell-language-server
              hlint
              apply-refact
              ormolu
            ];
            pathsToLink = [ "/bin" "/share/man" ];
          };
      in writeShellScriptBin "hs" ''
           export PATH=${myHaskellEnv}/bin:$PATH
           export MANPATH=${myHaskellEnv}/share/man:$MANPATH

           case $# in
             [1-9]*) exec "$@" ;;
           esac
         '';

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
    myEmacs = emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
      lsp-mode
      lsp-ui
      lsp-haskell
      (writeTextDir "share/emacs/site-lisp/default.el" ''
        (setq lsp-keymap-prefix "c-l")

        (require 'lsp-mode)
        (require 'lsp)
        (require 'lsp-haskell)
        ;; Hooks so haskell and literate haskell major modes trigger LSP setup
        (add-hook 'haskell-mode-hook #'lsp)
        (add-hook 'haskell-literate-mode-hook #'lsp)
      '')
    ]));
  };
}

