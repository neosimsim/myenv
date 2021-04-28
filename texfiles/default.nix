{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs.pkgs;
stdenv.mkDerivation {
  name = "texfiles";
  src = lib.sourceByRegex ./. [
    "Makefile"
    "^.*\.tex"
    "^.*\.sty"
    "^.*\.cls"
  ];

  # https://github.com/NixOS/nixpkgs/issues/24485
  FONTCONFIG_FILE = makeFontsConf { fontDirectories = [
    dejavu_fonts
    freefont_ttf
  ]; };

  buildInputs = [
    (texlive.combine {
      inherit (texlive)
        scheme-basic
        xetex
        unicode-math
        xcolor
        pgf # tikz
        blindtext
        etoolbox
        hyphenat
        lastpage
        german
        xstring
        newfloat
        lipsum
        float
      ;
    })
  ];

  installPhase = ''
    make PREFIX=$out install
  '';
}