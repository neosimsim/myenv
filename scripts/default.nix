{ pkgs }:
pkgs.stdenv.mkDerivation {
  name = "posix-scripts";
  src = ./.;
  buildPhase = "true";
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    make PREFIX=$out install

    wrapProgram $out/bin/find-match --set PATH ${with pkgs; lib.makeBinPath [ ripgrep ]}
    wrapProgram $out/bin/find-ex-module --set PATH ${with pkgs; lib.makeBinPath [ ripgrep ]}
    wrapProgram $out/bin/find-ex-function --prefix PATH : ${with pkgs; lib.makeBinPath [ findutils ripgrep ]}
  '';
}

