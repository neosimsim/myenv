{
  description = "NeoSimSim: my-packages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    {
      packages.x86_64-linux =
        {
          my-packages = import self { enableGui = false; pkgs = nixpkgs.legacyPackages.x86_64-linux; };
          my-gui-packages = import self { enableGui = true; pkgs = nixpkgs.legacyPackages.x86_64-linux; };
        };
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.my-packages;
    };
}
