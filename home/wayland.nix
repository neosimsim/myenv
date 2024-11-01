{ pkgs, lib, config, ... }: {
  options.myenv.wayland.enable = lib.mkEnableOption ''
    Configure Wayland.
  '';

  config = lib.mkIf config.myenv.wayland.enable {
    home.packages = with pkgs; [
      wl-clipboard-rs
    ];

    # use emacs Pure GTK to make use of Wayland scaling
    programs.emacs.package = pkgs.emacs-pgtk;
    home.sessionVariables.EDITOR = "emacsclient -a ''";
  };
}
