{
  description = "NeoSimSim: my-packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";

    plan9fansGo = {
      url = "github:9fans/go";
      flake = false;
    };

    goTools = {
      url = "github:golang/tools";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, plan9fansGo, goTools }:
    let pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    in
    {
      packages.x86_64-linux = {
        packagesWithoutGui = import self { inherit pkgs; enableGui = false; };
        packagesWithGui = import self { inherit pkgs; enableGui = true; };
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.packagesWithoutGui;

      devShells.x86_64-linux.neosimsim-shell = pkgs.haskellPackages.shellFor {
        packages = p: with p; [ neosimsim-shell ];
      };

      nixosModules.home-manager = import ./home.nix;

      overlay = final: prev: {

        haskellPackages = with prev;
          (haskellPackages.override {
            overrides = finalHspkgs: prevHspkgs: {
              xmonad = finalHspkgs.callHackageDirect
                {
                  pkg = "xmonad";
                  ver = "0.17.0";
                  sha256 = "sha256-zXw2qcqeU/f7edpiC1ZZCiUeKaRUINbqZ6Nhc70y4QQ=";
                }
                { };
              xmonad-contrib = finalHspkgs.callHackageDirect
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

        editinacme = final.buildGoModule {
          name = "editinacme";

          src = plan9fansGo;

          vendorSha256 = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

          buildPhase = ''
            go install 9fans.net/go/acme/editinacme
          '';

          meta = with nixpkgs.lib; {
            homepage = "https://github.com/9fans/go";
            license = licenses.mit;
            platforms = platforms.linux ++ platforms.darwin;
          };
        };

        acmego = final.buildGoModule {
          name = "acmego";

          src = plan9fansGo;

          vendorSha256 = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

          buildPhase = ''
            go install 9fans.net/go/acme/acmego
          '';

          meta = with nixpkgs.lib; {
            homepage = "https://github.com/9fans/go";
            license = licenses.mit;
            platforms = platforms.linux ++ platforms.darwin;
          };
        };

        Watch = final.buildGoModule {
          name = "Watch";

          src = plan9fansGo;

          vendorSha256 = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

          buildPhase = ''
            go install 9fans.net/go/acme/Watch
          '';

          meta = with nixpkgs.lib; {
            homepage = "https://github.com/9fans/go";
            license = licenses.mit;
            platforms = platforms.linux ++ platforms.darwin;
          };
        };

        goimports = final.buildGoModule {
          name = "goimports";

          src = goTools;

          vendorSha256 = "sha256-DvRtSBvp+5Dns26bWpu5+bbkaJBTCywd7mNDzox18AM=";

          proxyVendor = true;

          buildPhase = ''
            go install golang.org/x/tools/cmd/goimports
          '';

          meta = with nixpkgs.lib; {
            homepage = "https://pkg.go.dev/golang.org/x/tools/cmd/goimports";
            license = licenses.bsd3;
            platforms = platforms.linux ++ platforms.darwin;
          };
        };

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

      checks.x86_64-linux = {
        default = self.defaultPackage.x86_64-linux;

        packagesWithoutGui = self.packages.x86_64-linux.packagesWithoutGui;
        packagesWithGui = self.packages.x86_64-linux.packagesWithGui;
        ma = self.packages.x86_64-linux.packagesWithGui.ma;

        nixosWithGui = self.nixosConfigurations.withGui.config.system.build.toplevel;
        nixosWithoutGui = self.nixosConfigurations.withoutGui.config.system.build.toplevel;

        testCommandsWithGui = pkgs.runCommand "test-commands-with-gui"
          {
            buildInputs = [
              pkgs.which
              self.packages.x86_64-linux.packagesWithGui
            ];
          } ''
          which xmonad >$out
        '';

        testCommandsWithoutGui = pkgs.runCommand "test-commands-without-gui"
          {
            buildInputs = [
              pkgs.which
              self.packages.x86_64-linux.packagesWithoutGui
            ];
          } ''
          ! which xmonad >$out
        '';
      };
    };
}
