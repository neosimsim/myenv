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
    , gosec
    , goTools
    , home-manager
    , nixpkgs
    , hookmark
    , nur
    , passage
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
            packages = {
              packagesWithoutGui = import ./nix/packages.nix { inherit pkgs; enableGui = false; };
              packagesWithGui = import ./nix/packages.nix { inherit pkgs; enableGui = true; };
            };

            devShells.neosimsim-shell = pkgs.haskellPackages.shellFor {
              packages = p: with p; [ neosimsim-shell ];
            };

            checks = {
              packagesWithoutGui = self.packages.x86_64-linux.packagesWithoutGui;
              packagesWithGui = self.packages.x86_64-linux.packagesWithGui;

              # disabled packages
              ma = self.packages.x86_64-linux.packagesWithGui.ma;
            };
          });
    in
    nixpkgs.lib.recursiveUpdate genericOutputs
      {
        packages.x86_64-linux =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;
              overlays = [
                self.overlays.default
                emacs-overlay.overlay
              ];
            };
          in
          {
            packagesWithXServer = import ./nix/packages.nix {
              inherit pkgs;
              useXServer = true;
            };
            packagesWithSway = import ./nix/packages.nix {
              inherit pkgs;
              useSway = true;
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

        homeConfigurations.macbook = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;

          modules = [
            ./nix/home.nix

            ({ pkgs, config, ... }: {
              nixpkgs.overlays = [
                self.overlays.default
                nur.overlay
                emacs-overlay.overlay
              ];

              home = {
                stateVersion = "22.05";
                username = "neosimsim";
                homeDirectory = "/Users/neosimsim";

                # Link manually until Home Manager enables it again
                # https://github.com/nix-community/home-manager/blob/db00b39a9abec04245486a01b236b8d9734c9ad0/modules/targets/darwin/linkapps.nix
                # https://github.com/nix-community/home-manager/issues/1341#issuecomment-687286866
                file."Applications/Home Manager Apps".source =
                  let
                    apps = pkgs.buildEnv {
                      name = "home-manager-applications";
                      paths = config.home.packages;
                      pathsToLink = "/Applications";
                    };
                  in
                  "${apps}/Applications";
              };

              myenv = {
                enable = true;
                enableGui = true;
              };
            })
          ];
        };

        overlays.default = import ./nix/overlay.nix inputs;

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
          in
          {
            nixosWithXServer = pkgs.runCommand "test-myenv-with-xserver"
              {
                nixRoot = withXServer.config.system.build.toplevel;
                homeFiles = withXServer.config.home-manager.users.neosimsim.home-files;
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
                nixRoot = withSway.config.system.build.toplevel;
                homeFiles = withSway.config.home-manager.users.neosimsim.home-files;
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
                nixRoot = withoutGui.config.system.build.toplevel;
                homeFiles = withoutGui.config.home-manager.users.neosimsim.home-files;
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
