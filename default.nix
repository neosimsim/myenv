{
  enableGui ? false,
  pkgs ? import <nixpkgs> { overlays = [ (import ./overlay.nix { inherit enableGui; }) ]; },
}:
with pkgs;
lib.lowPrio (buildEnv {
   name = "my-packages";
   paths = [
     # make sure nix-shell runs mksh
     (lib.hiPrio (writeShellScriptBin "nix-shell" ''
       exec ${nix}/bin/nix-shell --run ${mksh}/bin/mksh "$@"
     ''))
     (writeShellScriptBin "nixFlakes" ''
       exec ${nixUnstable}/bin/nix --experimental-features "nix-command flakes" "$@"
     '')
     # make sure we use stable nix even if unstable is installed globally
     nix

     ag
     #agda
     aspell_
     beamPackages.elixir
     cargo
     clippy
     emacs
     entr
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
     pass
     pinentry-curses
     plan9port
     nix-prefetch-scripts
     rustc
     rustfmt
     scripts
     texlive-full
     tmux
     typespeed
     vis
     unzip

     # packages needed to work at sonnen
     beamPackages.erlang
     gcc
     lsof
     moreutils
     slack
     niv
     # cockroachdb
     rabbitmq-server
   ] ++ (if enableGui then [
     alacritty
     chromium
     discord
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
     xfce.thunar
     wire-desktop
     xmobar
     xorg.xmodmap
     xorg.xkill
     xsel
     zathura
   ] else []) ++ (with haskellPackages; [
     (ghc.withHoogle (p: with p; [
       containers
       extra
       filepath
       haskell-language-server
       hspec
       QuickCheck
       raw-strings-qq
       regex-tdfa
       safe
       string-interpolate
     ] ++ (if enableGui then [
       xmonad
       xmonad-contrib
     ] else [])))
     apply-refact
     cabal2nix
     cabal-fmt
     cabal-install
     hasktags
     ormolu
     pandoc
     steeloverseer
   ]);
   pathsToLink = [ "/share/man" "/share/doc" "/share/terminfo" "/bin" "/etc"];
   extraOutputsToInstall = [ "man" "doc" ];
})