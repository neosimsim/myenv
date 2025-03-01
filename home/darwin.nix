{ pkgs, lib, config, ... }:
lib.mkIf (pkgs.stdenv.isDarwin && config.myenv.coreutils.enable) {

  home.packages = with pkgs; [
    # Add nix to ensure it matches this flake and so it's added to emacs PATH.
    nix
  ];

  programs.home-manager.enable = true;

  myenv.emacs.package = pkgs.emacs-git;

  home.sessionVariables.EDITOR = "emacsclient -a ''";

  targets.darwin.search = "DuckDuckGo";
}
