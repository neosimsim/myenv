{ pkgs ? import <nixpkgs> { }
, ghc ? "default"
, buildInputs ? [ ]
, cabalProjects ? [ ]
}:
let
  lib = pkgs.lib;

  cwd = builtins.getEnv "PWD";

  # checks if a .cabal file exists in cwd
  isTopLevelCabal = lib.attrsets.filterAttrs (name: _: lib.hasSuffix ".cabal" name) (builtins.readDir cwd) != { };

  haskellPaths =
    if isTopLevelCabal
    then
      { project = cwd; }
    else
      lib.filesystem.haskellPathsInDir cwd;

  projectNames =
    if cabalProjects != [ ] then
      cabalProjects
    else
      builtins.attrNames haskellPaths;

  haskellPackages =
    if ghc == "default" then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${ghc};

  haskellPackagesW = haskellPackages.override {
    overrides = pkgs.haskell.lib.packageSourceOverrides haskellPaths;
  };

in
haskellPackagesW.shellFor {
  inherit
    buildInputs
    ;

  packages = lib.attrVals projectNames;
}
