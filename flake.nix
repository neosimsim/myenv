{
  description = "NeoSimSim: my-packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager }:
    let pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    in
    {
      packages.x86_64-linux =
        {
          packagesWithoutGui = import self { inherit pkgs; enableGui = false; };
          packagesWithGui = import self { inherit pkgs; enableGui = true; };

          inherit (pkgs)
            haskellPackages
            ;
        };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.packagesWithoutGui;

      devShells.x86_64-linux.haskellPackages.neosimsim-shell = pkgs.haskellPackages.shellFor {
        packages = p: with p; [ neosimsim-shell ];
      };

      nixosModules.home-manager = import ./home.nix;

      overlay = self: super: {

        haskellPackages = with super; haskellPackages.extend (haskell.lib.packageSourceOverrides {
          neosimsim-shell = ./pkgs/shell;
        });

      };

      nixosConfigurations = {
        withoutGui = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ({ ... }: {
              nixpkgs.overlays = [ self.overlay ];
              boot.isContainer = true;
              users.users.neosimsim.isNormalUser = true;
            })
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.neosimsim = self.nixosModules.home-manager;
            }
          ];
        };

        withGui = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ({ ... }: {
              nixpkgs.overlays = [ self.overlay ];
              boot.isContainer = true;
              services.xserver.enable = true;
              users.users.neosimsim.isNormalUser = true;
            })
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.neosimsim = { ... }: {
                imports = [ self.nixosModules.home-manager ];
                myenv.enableGui = true;
              };
            }
          ];
        };
      };
    };
}
