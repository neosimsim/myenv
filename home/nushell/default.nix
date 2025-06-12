{ pkgs, ... }: {

  home.packages = with pkgs;[
    nushell
    nufmt
  ];

  programs.nushell = {
    enable = true;
    configFile.source = ./config.nu;
    environmentVariables = {
      EDITOR = "emacsclient -a ''";
    };
  };

  programs.carapace = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.konsole.profiles.Custom.command = "${pkgs.nushell}/bin/nu";
}
