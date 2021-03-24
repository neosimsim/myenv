{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs;
    let
      hconv = haskellPackages.callCabal2nix "hconv" (fetchgit {
        url = "https://gitlab.com/neosimsim/hconv.git";
        rev = "05b254dc4e2c9258f7d9a4721847376a001b99de";
        sha256 = "11wz8a3iq1x81kx7gw06iacdza8nvcdph3zb53lxmlsczc8dwqaq";}) {};

      hookmark = haskellPackages.callCabal2nix "hookmark" (fetchgit {
        url = "https://gitlab.com/neosimsim/hookmark.git";
        rev = "2e9e69dc4b12aaf8af50a2b5c053030501c0562c";
        sha256 = "1nhs0lhy802j5z1lh4m40rrmdcnk5d3shvdmn2ngfjzlg1pr67mg";}) {};

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
      aspell_ = aspellWithDicts(p: with p; [ en de ]);
      emacs_ =
        ((emacsPackagesGen emacs-nox).emacsWithPackages [
          (writeTextDir "/share/emacs/site-lisp/default.el" ''
            (setq create-lockfiles nil)
            (setq auto-save-default nil)
            (setq make-backup-files nil)
            (setq kill-whole-line t)
            (setq-default show-trailing-whitespace t)
            (setq-default indicate-empty-lines t)
            (setq-default indent-tabs-mode nil)
            (load-theme 'adwaita)
            (global-font-lock-mode 0)
            (show-paren-mode)

            (defun pipe-shell-region (cmd start end)
              "Pipe region to shell command and replace region with the output
            (combined stdout and stderr). The region is only replaced when the
            shell command exits 0, otherwise the commands output (combined
            stdout and stderr) in displayed in a new buffer."
              (defconst out-buffer (generate-new-buffer "pipe"))
              (setq exit-status (call-shell-region start end cmd nil out-buffer))
              (if (equal 0 exit-status)
                  (progn (delete-region start end)
                         (insert-buffer-substring out-buffer)
                         (kill-buffer out-buffer))
                (display-buffer out-buffer)))

            (global-set-key (kbd "C-c C-u") (lambda (&optional start end)
                                              (interactive "r")
                                              (pipe-shell-region "uni" start end)))

            (defvar formatter "sed 's/[[:blank:]]*$//'")

            (global-set-key (kbd "C-x M-f") (lambda ()
                                              (interactive)
                                              (defconst p (point))
                                              (pipe-shell-region formatter (point-min) (point-max))
                                              (goto-char p)))

            (defun haskell-setup ()
              (setq formatter "ormolu"))
            (add-hook 'haskell-mode-hook 'haskell-setup)

            (defun cabal-setup ()
              (setq formatter "cabal-fmt"))
            (add-hook 'cabal-mode-hook 'cabal-setup)

            (defun elixir-setup ()
              (setq formatter "mix format"))
            (add-hook 'elixir-mode-hook 'elixir-setup)

            (defun rust-setup ()
              (setq formatter "rustfmt"))
            (add-hook 'rust-mode-hook 'rust-setup)
          '')
        ]);
    in {
      myPackages = pkgs.buildEnv {
        name = "my-packages";
        paths = [
          # make sure nix-shell runs mksh
          (writeShellScriptBin "nix-shell" ''
            exec ${nix}/bin/nix-shell --run ${mksh}/bin/mksh "$@"
          '')

          agda
          aspell_
          beamPackages.elixir
          cargo
          clang
          clippy
          fzf
          git
          gnumake
          gnupg
          go
          hconv
          hookmark
          isync
          jq
          mutt
          emacs_
          pinentry-curses
          plan9port
          rustc
          rustfmt
          texlive.combined.scheme-full
          tmux
          typespeed
          vis
        ] ++ (with haskellPackages; [
          (ghc.withPackages (p: with p; [
            containers
            extra
            filepath
            hspec
            QuickCheck
            raw-strings-qq
            regex-tdfa
            safe
            string-interpolate
            xmonad
            xmonad-contrib
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
          discord
          dmenu
          feh
          firefox
          haskellPackages.threadscope
          klavaro
          signal-desktop
          wire-desktop
          xmobar
          xorg.xmodmap
          rxvt-unicode
        ];
        pathsToLink = [ "/share/man" "/share/doc" "/share/terminfo" "/bin" "/etc"];
        extraOutputsToInstall = [ "man" "doc" ];
      };
    };
}

