{ pkgs, ghc ? "default" }:
let
  hsPkgs = with pkgs; if ghc == "default"
  then haskellPackages
  else haskell.packages.${ghc};
in
{

  haskellPackages = hsPkgs.extend (pkgs.haskell.lib.packageSourceOverrides {
    scripts = (pkgs.lib.sourceByRegex ./. [
      "^.*\.md$"
      "^.*\.hs$"
      "^scripts\.cabal$"
      "^hsSrc.*$"
      "^hsTest.*$"
      "^hsMain.*$"
    ]);
  });

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
