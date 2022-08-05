{
  description = "NeoSimSim: my-packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:neosimsim/home-manager/fish-extra-outputs-to-install-completions";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
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
          };
          aarch-darwin = {
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
              imports = [ (import ./nix/home.nix) ];

              home.stateVersion = "22.05";

              myenv = {
                enable = true;
                useXServer = config.services.xserver.enable;
                useSway = config.programs.sway.enable;
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
              ./nix/home.nix

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
                  enableGui = true;
                };
              })
            ];
          };

          macbookWithoutGui = home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.aarch64-darwin;

            modules = [
              ./nix/home.nix

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
                  enableGui = false;
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
            checkMissing = pkgs.writeShellScript "checkMissing" ''
              if [ -f $1 ]; then
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
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/xmonad
              ${checkMissing} $nixRoot/etc/profiles/per-user/neosimsim/bin/sway
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/firefox
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/Afmt

              ${checkPresent} $homeFiles/.config/git/config
              ${checkPresent} $homeFiles/.config/xmobar/xmobar.hs
              ${checkPresent} $homeFiles/.config/xmobar/xmobar
              ${checkPresent} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
              ${checkPresent} $homeFiles/lib/plumbing

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
              ${checkPresent} $nixRoot/etc/profiles/per-user/neosimsim/bin/firefox
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

              ${checkPresent} $homeFiles/.config/git/config
              ${checkMissing} $homeFiles/.Xresources
              ${checkMissing} $homeFiles/.config/sway/config

              echo successful >$out
            '';
          };
      };
}
