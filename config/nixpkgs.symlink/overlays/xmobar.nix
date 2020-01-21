# https://discourse.nixos.org/t/haskell-how-to-override-cabal-flags/2990
self: super: {
  myXmobar = self.haskellPackages.xmobar.overrideAttrs (oldAttrs: rec {
     configureFlags = [
       "-fwith_utf8"
       "-fwith_xft"
       "-fwith_alsa"
       ];
   });
}
