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
          packagesWithoutGui = import ./pkgs.nix { inherit pkgs; enableGui = false; };
          packagesWithGui = import ./pkgs.nix { inherit pkgs; enableGui = true; };

          environmentWithoutGui = import self { pkgs = pkgs; enableGui = false; };
          environmentWithGui = import self { pkgs = pkgs; enableGui = true; };

          inherit (pkgs)
            haskellPackages
            ;
        };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.environmentWithoutGui;

      devShells.x86_64-linux.haskellPackages.neosimsim-shell = self.packages.x86_64-linux.haskellPackages.shellFor {
        packages = p: with p; [ neosimsim-shell ];
      };

      nixosModules.home-manager = import ./home.nix;

      overlay = self: super: {

        haskellPackages = with super;
          (haskellPackages.override {
            overrides = selfHspkgs: superHspkgs: {
              xmonad = selfHspkgs.callHackageDirect
                {
                  pkg = "xmonad";
                  ver = "0.17.0";
                  sha256 = "sha256-zXw2qcqeU/f7edpiC1ZZCiUeKaRUINbqZ6Nhc70y4QQ=";
                }
                { };
              xmonad-contrib = selfHspkgs.callHackageDirect
                {
                  pkg = "xmonad-contrib";
                  ver = "0.17.0";
                  sha256 = "sha256-MaskRCiMtS4hhTpkxfySMiQ3QsIUkiEPEjgDnoWA7GM=";
                }
                { };
            };
          }).extend (haskell.lib.packageSourceOverrides {
            neosimsim-shell = ./tools/shell;

            hconv = fetchgit {
              url = "https://gitlab.com/neosimsim/hconv.git";
              rev = "05b254dc4e2c9258f7d9a4721847376a001b99de";
              sha256 = "11wz8a3iq1x81kx7gw06iacdza8nvcdph3zb53lxmlsczc8dwqaq";
            };

            hookmark = fetchgit {
              url = "https://gitlab.com/neosimsim/hookmark.git";
              rev = "2e9e69dc4b12aaf8af50a2b5c053030501c0562c";
              sha256 = "1nhs0lhy802j5z1lh4m40rrmdcnk5d3shvdmn2ngfjzlg1pr67mg";
            };
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
              home-manager.users.neosimsim = { ... }: {
                imports = [ self.nixosModules.home-manager ];
                myenv.enable = true;
              };
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
                myenv.enable = true;
                myenv.enableGui = true;
              };
            }
          ];
        };
      };
    };
}
