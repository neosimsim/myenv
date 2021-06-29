{
  description = "NeoSimSim: my-packages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    {
      packages.x86_64-linux =
        {
          packagesWithoutGui = import self { enableGui = false; pkgs = nixpkgs.legacyPackages.x86_64-linux; };
          packagesWithGui = import self { enableGui = true; pkgs = nixpkgs.legacyPackages.x86_64-linux; };
        };
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.packages;
      nixosModule = { config, pkgs, ... }: {
        users.users.neosimsim.packages = [ (import self { inherit pkgs; enableGui = config.services.xserver.enable; }) ];
      };
    };
}
