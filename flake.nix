{
  description = "NeoSimSim: my-packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager }:
    {
      packages.x86_64-linux =
        let pkgs = import nixpkgs {
          system = "x86_64-linux";
          overlays = [ self.overlay ];
        };
        in
        {
          packagesWithoutGui = import self { inherit pkgs; enableGui = false; };
          packagesWithGui = import self { inherit pkgs; enableGui = true; };
        };
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.packagesWithoutGui;

      nixosModules.home-manager = import ./home.nix;

      overlay = self: super: {
        emacsPackagesFor = emacs: ((super.emacsPackagesFor emacs).overrideScope' (self: super: {
          # spinner for emacs-nox isn't cached and nix want to rebuild (fetch) it but
          # version 1.7.3 as pinned by nixpkgs has been removed in favor of 1.7.4.
          spinner = super.spinner.override {
            elpaBuild = args: super.elpaBuild (args // {
              version = "1.7.4";
              src = builtins.fetchurl {
                url = "https://elpa.gnu.org/packages/spinner-${self.spinner.version}.tar";
                sha256 = "140kss25ijbwf8hzflbjz67ry76w2cyrh02axk95n6qcxv7jr7pv";
              };
            });
          };
        }));
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
