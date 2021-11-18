{ enableGui ? false
, pkgs
}:
pkgs.buildEnv {
  name = "my-packages";
  paths = pkgs.lib.attrValues (import ./pkgs.nix { inherit enableGui pkgs; });
  pathsToLink = [ "/share/man" "/share/doc" "/share/info" "/share/terminfo" "/bin" "/etc" ];
  extraOutputsToInstall = [ "man" "doc" ];
}
