{ pkgs, lib, config, ... }: {
  options.myenv.manageWayland = lib.mkOption {
    type = lib.types.bool;
    default = false;
  };

  config = lib.mkIf config.myenv.manageWayland {
    home.packages = with pkgs; [
      wl-clipboard-rs
    ];

    # use emacs Pure GTK to make use of Wayland scaling
    programs.emacs.package = pkgs.emacs-pgtk;
    home.sessionVariables.EDITOR = "emacsclient -a ''";
  };
}
