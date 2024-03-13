{ pkgs, lib, ... }: lib.mkIf pkgs.stdenv.isDarwin {

  programs.home-manager.enable = true;

  targets.darwin.search = "DuckDuckGo";

  myenv.enable = true;
  myenv.enableGuiTools = true;
}
