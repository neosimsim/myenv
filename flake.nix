{
  description = "NeoSimSim: my-packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    nur.url = "github:nix-community/NUR";

    plan9fansGo = {
      url = "github:9fans/go";
      flake = false;
    };

    hookmark = {
      url = "gitlab:neosimsim/hookmark";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs =
    { self
    , emacs-overlay
    , flake-utils
    , home-manager
    , nixpkgs
    , hookmark
    , nur
    , plan9fansGo
    , plasma-manager
    }@inputs:
    let
      genericOutputs = with flake-utils.lib; eachSystem [ system.x86_64-linux system.aarch64-darwin ]
        (system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [
                self.overlays.default
                emacs-overlay.overlay
              ];
            };
          in
          {
            devShells = {
              neosimsim-shell = pkgs.haskellPackages.shellFor {
                packages = p: with p; [ neosimsim-shell ];
              };

              xmonad = pkgs.mkShell {
                packages = [
                  (pkgs.haskellPackages.ghcWithPackages (p: with p;[
                    xmonad
                    xmonad-contrib
                  ]))
                ];
              };
            };

            checks = {
              # packages I added in overlay but not in home.packages:
              ma = pkgs.ma;
            };
          });
    in
    nixpkgs.lib.recursiveUpdate genericOutputs
      {
        packages = {
          x86_64-linux = {
            pathWithGui = self.nixosConfigurations.withXServer.config.home-manager.users.neosimsim.home.path;
            pathWithoutGui = self.nixosConfigurations.withoutGui.config.home-manager.users.neosimsim.home.path;

            emacs = with (import nixpkgs { system = "x86_64-linux"; }).lib;
              lists.findSingle
                (x: strings.hasPrefix "emacs" x.name)
                (builtins.throw "no emacs found")
                (builtins.throw "more than one emacs found")
                self.nixosConfigurations.withXServer.config.home-manager.users.neosimsim.home.packages;

          };
          aarch64-darwin = {
            pathWithGui = self.homeConfigurations.macbook.config.home.path;
            pathWithoutGui = self.homeConfigurations.macbookWithoutGui.config.home.path;
          };
        };

        nixosModules.default = { config, ... }: {

          imports = [
            home-manager.nixosModules.home-manager
          ];

          nixpkgs.overlays = [
            self.overlays.default
            nur.overlay
            emacs-overlay.overlay
          ];

          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;

            users.neosimsim = { ... }: {
              imports = [
                plasma-manager.homeManagerModules.plasma-manager
                (import ./nix/home)
              ];

              home.stateVersion = "22.05";

              myenv = {
                enable = true;
                manageXsession = config.services.xserver.enable;
                manageSway = config.programs.sway.enable;
                managePlasma5 = config.services.xserver.desktopManager.plasma5.enable;
              };
            };
          };
        };

        # NixOS configurations for testing
        nixosConfigurations = {

          withoutGui = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              ({ ... }: {
                boot.isContainer = true;
                users.users.neosimsim.isNormalUser = true;
                system.stateVersion = "22.05";
              })
              self.nixosModules.default
            ];
          };

          withXServer = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              ({ ... }: {
                boot.isContainer = true;
                services.xserver.enable = true;
                users.users.neosimsim.isNormalUser = true;
                system.stateVersion = "22.05";
              })
              self.nixosModules.default
            ];
          };

          withXmonad = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              ({ ... }: {
                boot.isContainer = true;
                services.xserver.enable = true;
                users.users.neosimsim.isNormalUser = true;
                system.stateVersion = "22.05";
                home-manager.users.neosimsim.myenv.manageXmonad = true;
              })
              self.nixosModules.default
            ];
          };

          withSway = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              ({ ... }: {
                boot.isContainer = true;
                programs.sway.enable = true;
                users.users.neosimsim.isNormalUser = true;
                system.stateVersion = "22.05";
              })
              self.nixosModules.default
            ];
          };
        };

        homeConfigurations = {
          macbook = home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.aarch64-darwin;

            modules = [
              plasma-manager.homeManagerModules.plasma-manager
              ./nix/home

              ({ pkgs, config, ... }: {
                nixpkgs.overlays = [
                  self.overlays.default
                  emacs-overlay.overlay
                ];

                home = {
                  stateVersion = "22.05";
                  username = "neosimsim";
                  homeDirectory = "/Users/neosimsim";
                };

                programs.home-manager.enable = true;

                myenv = {
                  enable = true;
                  enableGuiTools = true;
                };
              })
            ];
          };

          macbookWithoutGui = home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.aarch64-darwin;

            modules = [
              plasma-manager.homeManagerModules.plasma-manager
              ./nix/home

              ({ pkgs, config, ... }: {
                nixpkgs.overlays = [
                  self.overlays.default
                  emacs-overlay.overlay
                ];

                home = {
                  stateVersion = "22.05";
                  username = "neosimsim";
                  homeDirectory = "/Users/neosimsim";
                };

                myenv = {
                  enable = true;
                  enableGuiTools = false;
                };
              })
            ];
          };
        };

        overlays.default = import ./nix/overlay.nix inputs;

        checks.aarch64-darwin.macbook = self.homeConfigurations.macbook.config.home.path;

        checks.x86_64-linux =
          let
            pkgs = import nixpkgs {
              system = "x86_64-linux";
            };
            checkPresent = pkgs.writeShellScript "checkPresent" ''
              if ! [ -f $1 ]; then
                echo missing $1 >&2
                exit 1
              fi
            '';
            checkPresentDir = pkgs.writeShellScript "checkPresent" ''
              if ! [ -d $1 ]; then
                echo missing $1 >&2
                exit 1
              fi
            '';
            checkMissing = pkgs.writeShellScript "checkMissing" ''
              if [ -e $1 ]; then
                echo unexpected $1 >&2
                exit 1
              fi
            '';
          in
          {
            nixosWithXServer = pkgs.runCommand "test-myenv-with-xserver"
              {
                nixRoot = self.nixosConfigurations.withXServer.config.system.build.toplevel;
                homeFiles = self.nixosConfigurations.withXServer.config.home-manager.users.neosimsim.home-files;
              } ''
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/emacs
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/fm
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/do-the-thing
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/firefox
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/chromium
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/Afmt
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/xmonad
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/sway

              ${checkPresent} $homeFiles/.config/git/config
              ${checkPresent} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
              ${checkPresentDir} $homeFiles/.config/chromium
              ${checkPresent} $homeFiles/lib/plumbing
              ${checkMissing} $homeFiles/.config/xmobar/xmobar.hs
              ${checkMissing} $homeFiles/.config/xmobar/xmobar

              echo successful >$out
            '';

            nixosWithXmonad = pkgs.runCommand "test-myenv-with-xserver"
              {
                nixRoot = self.nixosConfigurations.withXmonad.config.system.build.toplevel;
                homeFiles = self.nixosConfigurations.withXmonad.config.home-manager.users.neosimsim.home-files;
              } ''
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/xmonad
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/sway

              ${checkPresent} $homeFiles/.config/xmobar/xmobar.hs
              ${checkPresent} $homeFiles/.config/xmobar/xmobar
              ${checkPresent} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.Xmodmap

              echo successful >$out
            '';

            nixosWithSway = pkgs.runCommand "test-myenv-with-sway"
              {
                nixRoot = self.nixosConfigurations.withSway.config.system.build.toplevel;
                homeFiles = self.nixosConfigurations.withSway.config.home-manager.users.neosimsim.home-files;
              } ''
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/emacs
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/fm
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/do-the-thing
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/xmonad
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/sway
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/Afmt

              ${checkPresent} $homeFiles/.config/git/config
              ${checkPresent} $homeFiles/.config/sway/config
              ${checkMissing} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
              ${checkPresent} $homeFiles/lib/plumbing

              echo successful >$out
            '';

            nixosWithoutGui = pkgs.runCommand "test-myenv-without-gui"
              {
                nixRoot = self.nixosConfigurations.withoutGui.config.system.build.toplevel;
                homeFiles = self.nixosConfigurations.withoutGui.config.home-manager.users.neosimsim.home-files;
              } ''
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/fm
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/do-the-thing
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/xmonad
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/firefox
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/chromium

              ${checkPresent} $homeFiles/.config/git/config
              ${checkMissing} $homeFiles/.Xresources
              ${checkMissing} $homeFiles/.config/sway/config
              ${checkMissing} $homeFiles/.mozilla/firefox/default/user.js
              ${checkMissing} $homeFiles/.config/chromium

              echo successful >$out
            '';
          };
      };
}
