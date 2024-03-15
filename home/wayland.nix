{ pkgs, lib, config, ... }: {
  options.myenv.useWayland = lib.mkOption {
    type = lib.types.bool;
    default = false;
  };

  config = lib.mkIf config.myenv.useWayland {
    home.packages = with pkgs; [
      wl-clipboard-rs
    ];
  };
}
