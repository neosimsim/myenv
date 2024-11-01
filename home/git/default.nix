{ pkgs, lib, ... }: {
  programs.git = {
    enable = true;
    package = lib.mkDefault pkgs.gitFull;
    attributes = [
      "*.gz diff=compressed"
      "*.bz2 diff=compressed"
      "*.xz diff=compressed"
    ];
    ignores = [
      "*.agda.vim"
      "*.latexmain"
      "*.local"
      ".cabal-sandbox"
      ".syntastic_*_config"
      ".sosrc"
      ".#*"
      "cabal.sandbox.config"
      "Acme"
      "result"
    ];
    includes = [
      { path = ./config; }
    ];
  };
}
