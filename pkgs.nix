{ enableGui ? false
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

in
{
  # make sure nix-shell runs mksh
  nix-shell-wrapped = (lib.hiPrio (writeShellScriptBin "nix-shell" ''
    exec ${nix}/bin/nix-shell --run ${mksh}/bin/mksh "$@"
  ''));

  # make sure we use unstable (until flakes become stable)
  nixFlakes = mkCommandAlias nixFlakes "nix" ''--experimental-features "nix-command flakes"'';

  inherit (pkgs)
    binutils
    dhall
    dhall-json
    entr
    fzf
    git-lfs
    gnumake
    gnupg
    go
    haskell-language-server
    htop
    isync
    jq
    libarchive
    mutt
    nixpkgs-fmt
    nix-prefetch-scripts
    pass
    pinentry-curses
    plan9port
    ripgrep
    tmux
    typespeed
    unzip
    vis
    ;

  scripts = import ./scripts { inherit pkgs; };

  texlive-full =
    let
      texfiles.pkgs = [ (import ./texfiles { inherit pkgs; }) ];
    in
    texlive.combine {
      inherit (texlive) scheme-full;
      inherit texfiles;
    };

  agda = pkgs.agda.withPackages (p: [ p.standard-library ]);

  fd = mkCommandAlias fd "fd" "--color never";

  aspell = aspellWithDicts (p: with p; [ en de ]);

  emacs =
    let
      emacs_ = with super; if enableGui then pkgs.emacs else emacs-nox;
    in
    (emacsPackagesGen emacs_).withPackages (epkgs: with epkgs; [
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
    typed-process
    QuickCheck
    raw-strings-qq
    regex-tdfa
    string-interpolate
    wreq

    # Preinstall common used lib to speed up nix builds.
    bifunctors
    hspec
    markdown-unlit
    profunctors
    safe
    semigroupoids
  ] ++ lib.optionals enableGui [
    xmonad
    xmonad-contrib
  ]);
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
    steeloverseer
    stylish-haskell
    ;
} // lib.optionalAttrs enableGui {

  inherit (pkgs)
    acmego
    alacritty
    brightnessctl
    dmenu
    editinacme
    feh
    klavaro
    mplayer
    numlockx
    rxvt-unicode
    scrot
    signal-desktop
    sxiv
    ungoogled-chromium
    Watch
    wire-desktop
    xsel
    zathura
    ;

  inherit (pkgs.xorg)
    xkill
    xmodmap
    ;

  inherit (pkgs.xfce)
    thunar
    ;

  inherit (pkgs.haskellPackages)
    threadscope
    ;

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
}
