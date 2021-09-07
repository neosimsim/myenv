{ pkgs, config, lib, ... }: with lib; {
  options.myenv = {
    enableGui = mkOption {
      type = types.bool;
      default = false;
    };
  };
  config = {
    home.packages =
      let
        myenv = with config.myenv; import ./. { inherit pkgs enableGui; };
      in
      [ myenv ];
  };
}
