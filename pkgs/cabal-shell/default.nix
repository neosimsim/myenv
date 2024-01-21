{ writeShellScriptBin }:
writeShellScriptBin "cabal-shell" ''
  nix-shell ${./shell.nix} --command $SHELL
''
