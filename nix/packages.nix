{ useXServer ? false
, useSway ? false
, enableGui ? useXServer || useSway
, pkgs
}:
with pkgs;
let

  mkCommandAlias = pkg: commandName: arguments:
    let
      alias = writeShellScriptBin "${commandName}" ''
        exec ${pkg}/bin/${commandName} ${arguments} "$@"
      '';
    in
    # Use buildEnv to ensure we have all binaries and docs installed, not just the alias.
    buildEnv {
      name = "${pkg.name}-with-alias";
      paths = [
        pkg
        (lib.hiPrio alias)
      ];
    };

  myEmacsConfig = pkgs.writeTextDir "share/emacs/site-lisp/default.el" (builtins.readFile ../dotfiles/emacs/init.el);

  packageSet = {
    # make sure nix-shell runs mksh
    nix-shell-wrapped = (lib.hiPrio (writeShellScriptBin "nix-shell" ''
      exec ${nix}/bin/nix-shell --run $SHELL "$@"
    ''));

    # make sure we use unstable (until flakes become stable)
    nixFlakes = mkCommandAlias nixFlakes "nix" ''--experimental-features "nix-command flakes"'';

    scripts = import ../scripts { inherit pkgs; };

    goScripts = import ../golang { inherit pkgs; };

    haskellScripts = (import ../haskell { inherit pkgs; }).scripts;

    cabalShell = pkgs.writeShellScriptBin "cabal-shell" ''
      nix-shell ${./cabal-shell.nix} --command $SHELL
    '';

    texlive-full =
      let
        texfiles.pkgs = [ (import ../texfiles { inherit pkgs; }) ];
      in
      texlive.combine {
        inherit (texlive) scheme-full;
        inherit texfiles;
      };

    inherit (pkgs)
      age
      dhall
      dhall-json
      entr
      fzf
      gcc
      git-lfs
      gnumake
      go
      goimports
      gosec
      #      haskell-language-server
      htop
      isync
      jq
      libarchive
      nixpkgs-fmt
      nix-prefetch-scripts
      passage
      plan9port
      ripgrep
      rnix-lsp
      tmux
      typespeed
      unzip
      ;

    agda = pkgs.agda.withPackages (p: [ p.standard-library ]);

    fd = mkCommandAlias fd "fd" "--color never";

    aspell = aspellWithDicts (p: with p; [ en de ]);

    emacs =
      let
        emacs_ =
          if stdenv.isDarwin
          then emacsGit
          else
            if enableGui
            # use emacs Pure GTK to make use of Wayland scaling
            then emacsPgtk
            else emacsGit-nox;
      in
      (emacsPackagesFor emacs_).withPackages (epkgs: with epkgs; [
        myEmacsConfig

        spacemacs-theme
        kaolin-themes
        ivy
        counsel
        amx
        highlight-symbol
        magit
        htmlize
        mixed-pitch
        avy
        buffer-move
        osm
        vimgolf

        haskell-mode
        nix-mode
        elm-mode
        elixir-mode
        rust-mode
        elisp-format

        lsp-mode
        lsp-ui
        flycheck
        company
        lsp-haskell
      ]);

    git = pkgs.git.override {
      guiSupport = enableGui;
    };

    ghc = haskellPackages.ghc.withHoogle (p: with p; [
      # Preinstall zlib to help cabal. Otherwise builds will
      # complain about missing zlib.h.
      zlib

      neosimsim-shell

      # Quality of life libraries for ghci
      aeson
      containers
      extra
      filelock
      filepath
      flow
      lens
      lens-aeson
      pidfile
      optparse-applicative
      typed-process
      QuickCheck
      raw-strings-qq
      regex-tdfa
      string-interpolate
      wreq

      # Preinstall common used lib to speed up nix builds.
      bifunctors
      concurrency
      dejafu
      hspec
      markdown-unlit
      profunctors
      safe
      semigroupoids
    ]);

    inherit (elmPackages)
      elm
      elm-format
      elm-language-server
      elm-review
      elm-test
      ;
  } // lib.mapAttrs (_: p: haskell.lib.justStaticExecutables p) {
    inherit (haskellPackages)
      apply-refact
      cabal2nix
      cabal-fmt
      cabal-install
      hconv
      hindent
      hlint
      hookmark
      ormolu
      pandoc
      stylish-haskell
      ;
  } // lib.optionalAttrs enableGui {
    inherit (pkgs)
      acmego
      alacritty
      editinacme
      ma
      meld
      mplayer
      Watch
      zathura
      ;
  } // lib.optionalAttrs (enableGui && pkgs.stdenv.isLinux) {
    inherit (pkgs)
      klavaro
      ;
  } // lib.optionalAttrs useXServer {
    inherit (pkgs)
      brightnessctl
      dmenu
      feh
      numlockx
      rxvt-unicode
      scrot
      signal-desktop
      sxiv
      xsel
      ;

    inherit (pkgs.xorg)
      xkill
      xmodmap
      ;

    inherit (pkgs.xfce)
      thunar
      ;

    inherit (pkgs.haskellPackages)
      xmobar
      ;
  } // lib.optionalAttrs useSway {
    inherit (pkgs)
      brightnessctl
      swaylock
      swayidle
      wl-clipboard
      mako
      wofi
      pulseaudio
      ;
  };
in
pkgs.buildEnv {
  name = "my-packages";
  paths = pkgs.lib.attrValues (builtins.removeAttrs packageSet [ "ma" ]);
  pathsToLink = [
    "/Applications"
    "/bin"
    "/etc"
    "/share/doc"
    "/share/fish/vendor_completions.d"
    "/share/fish/vendor_conf.d"
    "/share/fish/vendor_functions.d"
    "/share/info"
    "/share/man"
    "/share/terminfo"
  ];
  extraOutputsToInstall = [ "man" "doc" ];
  passthru = packageSet // {
    inherit myEmacsConfig;
  };
}
