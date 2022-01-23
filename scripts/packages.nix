{ pkgs ? import <nixpkgs> { }, ghc ? "default" }:
let
  hsPkgs = with pkgs; if ghc == "default"
  then haskellPackages
  else haskell.packages.${ghc};
in
{

  haskellPackages = hsPkgs.override {
    overrides = final: prev: {
      scripts = (prev.callCabal2nix "scripts"
        (pkgs.lib.sourceByRegex ./. [
          "^.*\.md$"
          "^.*\.hs$"
          "^scripts\.cabal$"
          ".*_test_in"
          ".*_test_out"
          "^hsSrc.*$"
          "^hsTest.*$"
          "^hsMain.*$"
        ])
        { }).overrideAttrs (oldAttrs: {
        checkPhase = ''
          ${oldAttrs.checkPhase}

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
  };

  posixScripts = pkgs.stdenv.mkDerivation {
    name = "posix-scripts";
    src = ./.;
    buildPhase = "true";
    buildInputs = [ pkgs.makeWrapper ];

    doCheck = true;
    checkPhase = ''
      make test-posix
    '';

    installPhase = ''
      make PREFIX=$out install-posix

      wrapProgram $out/bin/find-match --set PATH ${with pkgs; lib.makeBinPath [ ripgrep ]}
      wrapProgram $out/bin/find-ex-module --set PATH ${with pkgs; lib.makeBinPath [ ripgrep ]}
      wrapProgram $out/bin/find-ex-function --prefix PATH : ${with pkgs; lib.makeBinPath [ findutils ripgrep ]}
    '';
  };

  goScripts = pkgs.buildGoModule {
    name = "go-scripts";
    CGO_ENABLED = "0";

    src = (pkgs.lib.sourceByRegex ./. [
      "^go\.mod$"
      "^go\.sum$"
      "^.*\.go$"
      "^Afmt$"
    ]);

    vendorSha256 = "sha256-xEKB+i6bbkdYZMsH5jHLUacff6aGam1Pm0JQUw9IuZY=";
  };
}
