{
  description = "NeoSimSim: my-packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";

    hconv = {
      url = "gitlab:neosimsim/hconv";
      flake = false;
    };

    hookmark = {
      url = "gitlab:neosimsim/hookmark";
      flake = false;
    };

    plan9fansGo = {
      url = "github:9fans/go";
      flake = false;
    };

    goTools = {
      url = "github:golang/tools";
      flake = false;
    };

    gosec = {
      url = "github:securego/gosec";
      flake = false;
    };

    passage = {
      url = "github:FiloSottile/passage";
      flake = false;
    };
  };

  outputs =
    { self
    , gosec
    , goTools
    , hconv
    , home-manager
    , hookmark
    , nixpkgs
    , passage
    , plan9fansGo
    }:
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

            } // (haskell.lib.packageSourceOverrides
              {
                inherit
                  hconv
                  hookmark
                  ;

                neosimsim-shell = ./tools/shell;
              }
              finalHspkgs
              prevHspkgs);
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

        gosec = final.buildGoModule {
          name = "gosec";

          src = gosec;

          vendorSha256 = "sha256-ELfbdrMMeK6ZG+hnibhHNB+k/Zvkepl+cbUx+E/Dvr8=";

          buildPhase = ''
            go install github.com/securego/gosec/v2/cmd/gosec
          '';

          meta = with nixpkgs.lib; {
            homepage = "https://github.com/securego/gosec";
            license = licenses.apsl20;
            platforms = platforms.linux ++ platforms.darwin;
          };
        };

        passage = final.stdenv.mkDerivation {
          name = "passage";
          src = passage;

          buildPhase = ''true'';

          nativeBuildInputs = [ final.makeWrapper ];
          installPhase = ''
            make DESTDIR= PREFIX=$out install

            wrapProgram $out/bin/passage --prefix PATH : ${with final; lib.makeBinPath [ age tree xclip ]}
          '';
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
          (
          which Afmt
          which xmonad
          )>$out
        '';

        testCommandsWithoutGui = pkgs.runCommand "test-commands-without-gui"
          {
            buildInputs = [
              pkgs.which
              self.packages.x86_64-linux.packagesWithoutGui
            ];
          } ''
          (
          which fm
          which o
          ! which xmonad
          )>$out
        '';
      };
    };
}
