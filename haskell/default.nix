{ pkgs ? import <nixpkgs> { }, ghc ? "default" }:
let
  hsPkgs = with pkgs; if ghc == "default"
  then haskellPackages
  else haskell.packages.${ghc};
in
hsPkgs.override {
  overrides = final: prev: {
    scripts = (prev.callCabal2nix "scripts" ./. { }).overrideAttrs (oldAttrs: {
      checkInputs = with prev; [ cabal-fmt ormolu ];
      checkPhase = ''
        ${oldAttrs.checkPhase}

        make verify

        # TODO Move this to Setup.sh
        ./dist/build/uni/uni < uni_test_in | diff uni_test_out -
        echo test broken pipes
        ./dist/build/uni/uni < uni_test_in | sed 2q >/dev/null
        echo test print
        ./dist/build/uni/uni print | diff uni_print_test_out -
        echo test print broken pipes
        ./dist/build/uni/uni print | sed 2q >/dev/null
      '';
    });
  };
}

