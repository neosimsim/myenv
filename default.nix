{ enableGui ? false
, pkgs ? import <nixpkgs> { }
}:
with pkgs // import ./pkgs.nix { inherit enableGui pkgs; };
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
    dhall
    dhall-json
    emacs
    entr
    fzf
    git
    git-lfs
    gnumake
    gnupg
    go
    htop
    haskell-language-server
    isync
    jq
    libarchive
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
    ghc
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
    steeloverseer
    stylish-haskell
  ] ++ lib.optionals enableGui [
    alacritty
    brightnessctl
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
  ];

  pathsToLink = [ "/share/man" "/share/doc" "/share/info" "/share/terminfo" "/bin" "/etc" ];
  extraOutputsToInstall = [ "man" "doc" ];
}
