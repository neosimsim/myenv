{ pkgs, lib, config, ... }: {
  options.myenv.wayland.enable = lib.mkEnableOption ''
    Configure Wayland, i.e. GUI tools for GNU/Linux.
  '';

  config = lib.mkIf config.myenv.wayland.enable {
    home.packages = with pkgs; [
      wl-clipboard-rs
      librewolf
      gnuplot_qt
    ];

    # use emacs Pure GTK to make use of Wayland scaling
    myenv.emacs.package = pkgs.emacs-git-pgtk;
    home.sessionVariables.EDITOR = "emacsclient -a ''";
  };
}
