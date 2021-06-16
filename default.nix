{ enableGui ? false
, pkgs ? import (builtins.fetchTarball {
    name = "nixpkgs-unstable";
    url = "https://github.com/NixOS/nixpkgs/archive/6933d068c5d2fcff398e802f7c4e271bbdab6705.tar.gz";
    # git ls-remote https://github.com/nixos/nixpkgs nixos-unstable
    # nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/6933d068c5d2fcff398e802f7c4e271bbdab6705.tar.gz
    sha256 = "027axlv5g1l5sxkcnlswhmbc8y0xbgppggg9mhmfb78x7d8anqzq";
  }) { }
}:
with pkgs;
let
  scripts = import ./scripts { inherit pkgs; };

  hconv = haskellPackages.callCabal2nix "hconv"
    (fetchgit {
      url = "https://gitlab.com/neosimsim/hconv.git";
      rev = "05b254dc4e2c9258f7d9a4721847376a001b99de";
      sha256 = "11wz8a3iq1x81kx7gw06iacdza8nvcdph3zb53lxmlsczc8dwqaq";
    })
    { };

  hookmark = haskellPackages.callCabal2nix "hookmark"
    (fetchgit {
      url = "https://gitlab.com/neosimsim/hookmark.git";
      rev = "2e9e69dc4b12aaf8af50a2b5c053030501c0562c";
      sha256 = "1nhs0lhy802j5z1lh4m40rrmdcnk5d3shvdmn2ngfjzlg1pr67mg";
    })
    { };

  texfiles.pkgs = [ (import ./texfiles { inherit pkgs; }) ];
  texlive-full = texlive.combine {
    inherit (texlive) scheme-full;
    inherit texfiles;
  };

  agda = pkgs.agda.withPackages (p: [ p.standard-library ]);

  ma = stdenv.mkDerivation rec {
    pname = "ma";
    version = "11_2019-03-16";
    src = fetchTarball {
      url = "http://www.call-with-current-continuation.org/ma/ma.tar.gz";
      sha256 = "0g0lqijkwg5p0586spli2jd1yh0im0ma4fnhkf8mizhyrsj7ga2s";
    };
    cmds = [
      "awd"
      "B"
      "ma"
      "ma-eval"
      "plumb"
      "pty"
      "win"
    ];

    buildInputs = [ tk ];
    buildPhase = "./build";
    installPhase = ''
      mkdir -p $out/bin
      cp ${lib.concatStringsSep " " cmds} $out/bin
    '';
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
    in
    writeShellScriptBin "xmobar" ''
      export LD_LIBRARY_PATH=${alsaPlugins}/lib/alsa-lib
      exec ${myXmobar}/bin/xmobar
    '';
  aspell = aspellWithDicts (p: with p; [ en de ]);
  emacs =
    let
      emacs_ = with super; if enableGui then pkgs.emacs else emacs-nox;
    in
    ((emacsPackagesGen emacs_).emacsWithPackages (epkgs: with epkgs.melpaPackages; [
      acme-theme
      buffer-move
      multiple-cursors

      nix-mode
      elixir-mode
      rust-mode
      elisp-format

      lsp-mode
      lsp-ui
      flycheck
      company
      avy
      lsp-haskell
    ]));
in
buildEnv {
  name = "my-packages";
  paths = [
    # make sure nix-shell runs mksh
    (lib.hiPrio (writeShellScriptBin "nix-shell" ''
      exec ${nix}/bin/nix-shell --run ${mksh}/bin/mksh "$@"
    ''))
    # make sure we use unstable (until flakes become stable)
    nixUnstable

    ag
    agda
    aspell
    binutils
    emacs
    entr
    fzf
    git
    gnumake
    gnupg
    go
    hconv
    hookmark
    haskell-language-server
    isync
    jq
    mutt
    nixpkgs-fmt
    nix-prefetch-scripts
    pass
    pinentry-curses
    plan9port
    scripts
    texlive-full
    tmux
    typespeed
    unzip
    vis
  ] ++ (if enableGui then [
    alacritty
    ungoogled-chromium
    dmenu
    feh
    haskellPackages.threadscope
    klavaro
    mplayer
    numlockx
    rxvt-unicode
    scrot
    signal-desktop
    sxiv
    wire-desktop
    xfce.thunar
    xmobar
    xorg.xkill
    xorg.xmodmap
    xsel
    zathura
  ] else [ ]) ++ (with haskellPackages; [
    (ghc.withHoogle (p: with p; [
      # Preinstall zlib to help cabal. Otherwise builds will
      # complain about missing zlib.h.
      zlib

      containers
      extra
      filepath
      hlint
      hspec
      QuickCheck
      raw-strings-qq
      regex-tdfa
      safe
      string-interpolate
    ] ++ (if enableGui then [
      xmonad
      xmonad-contrib
    ] else [ ])))
    apply-refact
    cabal2nix
    cabal-fmt
    cabal-install
    hasktags
    ormolu
    pandoc
    steeloverseer
  ]);
  pathsToLink = [ "/share/man" "/share/doc" "/share/terminfo" "/bin" "/etc" ];
  extraOutputsToInstall = [ "man" "doc" ];
}
