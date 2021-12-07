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
    name = "scripts";
    src = ./.;
    buildPhase = "true";
    buildInputs = [ pkgs.makeWrapper ];
    installPhase = ''
      make PREFIX=$out install-posix

      sed -i '1c#!${pkgs.plan9port}/plan9/bin/rc' \
        $out/bin/Hsfmt

      wrapProgram $out/bin/find-match --set PATH ${with pkgs; lib.makeBinPath [ ripgrep ]}
      wrapProgram $out/bin/find-ex-module --set PATH ${with pkgs; lib.makeBinPath [ ripgrep ]}
      wrapProgram $out/bin/find-ex-function --prefix PATH : ${with pkgs; lib.makeBinPath [ findutils ripgrep ]}
    '';
  };
}
