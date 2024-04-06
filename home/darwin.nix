{ pkgs, lib, config, ... }: lib.mkIf pkgs.stdenv.isDarwin {

  home.packages = with pkgs; [
    # Add nix to ensure it matches this flake and so it's added to emacs PATH.
    nix
  ];

  programs.home-manager.enable = true;
  programs.emacs.extraConfig = ''
    (setq explicit-shell-file-name "${config.programs.fish.package}/bin/fish")
    (setenv "PATH" (concat "${config.home.profileDirectory}/bin:" (getenv "PATH")))
    (setq exec-path (append '("${config.home.profileDirectory}/bin") exec-path))
  '';

  targets.darwin.search = "DuckDuckGo";
}
