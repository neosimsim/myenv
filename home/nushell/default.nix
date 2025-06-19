{ pkgs, config, ... }: {

  home.packages = with pkgs;[
    nushell
    nufmt
    vivid
  ];

  programs.nushell = {
    enable = true;
    configFile.source = ./config.nu;
    environmentVariables = config.home.sessionVariables;
  };

  programs.carapace = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.konsole.profiles.Custom.command = "${pkgs.nushell}/bin/nu";
}
