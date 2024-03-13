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

      inherit (nixpkgs.lib.strings)
        concatStrings
        replaceStrings
        getName
        ;

      inherit (nixpkgs.lib.attrsets)
        listToAttrs
        ;

      setByName = list: listToAttrs (map
        (x: {
          name = (replaceStrings [ "." ] [ "-" ] (getName x));
          value = x;
        })
        list);

    in

    with flake-utils.lib; eachSystem [ system.x86_64-linux system.aarch64-darwin ]
      (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
            emacs-overlay.overlays.default
          ];
        };
      in
      {
        checks = self.packages.${system} //
          (with nixpkgs.lib; mapAttrs'
            (name: pkgs: nameValuePair ("${name}-home-files") (pkgs.home-files))
            self.packages.${system}) // nixpkgs.lib.optionalAttrs (system == flake-utils.lib.system.x86_64-linux) (
          let
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
            # packages I added in overlay but not in home.packages:
            ma = pkgs.ma;

            checkWithPlasma = pkgs.runCommand "test-myenv-with-plasma5"
              rec {
                path = self.packages.x86_64-linux.withPlasma;
                homeFiles = path.home-files;
              } ''
              ${checkPresent} $path/bin/emacs
              ${checkPresent} $path/bin/fm
              ${checkPresent} $path/bin/do-the-thing
              ${checkPresent} $path/bin/firefox-esr
              ${checkPresent} $path/bin/chromium
              ${checkPresent} $path/bin/Afmt
              ${checkPresent} $path/bin/mplayer
              ${checkPresent} $path/bin/ghc
              ${checkPresent} $path/bin/haskell-language-server-wrapper
              ${checkMissing} $path/bin/sway

              ${checkPresent} $homeFiles/.config/git/config
              ${checkPresent} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
              ${checkPresent} $homeFiles/.ghci
              ${checkPresentDir} $homeFiles/.config/chromium
              ${checkPresent} $homeFiles/lib/plumbing

              echo successful >$out
            '';

            checkWithSway = pkgs.runCommand "test-myenv-with-sway"
              rec {
                path = self.packages.x86_64-linux.withSway;
                homeFiles = path.home-files;
              } ''
              ${checkPresent} $path/bin/emacs
              ${checkPresent} $path/bin/fm
              ${checkPresent} $path/bin/do-the-thing
              ${checkPresent} $path/bin/sway
              ${checkPresent} $path/bin/Afmt

              ${checkPresent} $homeFiles/.config/git/config
              ${checkPresent} $homeFiles/.config/sway/config
              ${checkMissing} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
              ${checkPresent} $homeFiles/lib/plumbing

              echo successful >$out
            '';

            checkNoX = pkgs.runCommand "test-myenv-noX"
              rec {
                path = self.packages.x86_64-linux.noX;
                homeFiles = path.home-files;
              } ''
              ${checkPresent} $path/bin/fm
              ${checkPresent} $path/bin/do-the-thing
              ${checkMissing} $path/bin/firefox-esr
              ${checkMissing} $path/bin/chromium
              ${checkMissing} $path/bin/ghc

              ${checkPresent} $homeFiles/.config/git/config
              ${checkMissing} $homeFiles/.Xresources
              ${checkMissing} $homeFiles/.config/sway/config
              ${checkMissing} $homeFiles/.mozilla/firefox/default/user.js
              ${checkMissing} $homeFiles/.config/chromium

              echo successful >$out
            '';
          }
        );
      }) // (
      let
        mkHomePackage = homeConfig:

          homeConfig.config.home.path // {
            inherit (homeConfig.config) home-files;

            pkgs = setByName homeConfig.config.home.packages;
          };

        homePackage = system: myenv:
          let
            homeConfig = home-manager.lib.homeManagerConfiguration {
              pkgs = nixpkgs.legacyPackages.${system};

              modules = [
                plasma-manager.homeManagerModules.plasma-manager
                ./home

                ({ pkgs, config, ... }: {
                  nixpkgs.overlays = [
                    self.overlays.default
                    nur.overlay
                    emacs-overlay.overlay
                  ];

                  home = {
                    stateVersion = "22.05";
                    username = "neosimsim";
                    homeDirectory = "/home/neosimsim";
                  };

                  programs.home-manager.enable = true;

                  inherit myenv;

                })
              ];
            };
          in
          mkHomePackage homeConfig;
      in
      {
        packages.x86_64-linux.withPlasma = homePackage "x86_64-linux" {
          enable = true;
          enableGuiTools = true;
          managePlasma = true;
          ghc.enable = true;
        };

        packages.x86_64-linux.withPlasmaWayland = homePackage "x86_64-linux" {
          enable = true;
          enableGuiTools = true;
          managePlasma = true;
          useWayland = true;
          ghc.enable = true;
        };

        packages.x86_64-linux.withSway = homePackage "x86_64-linux" {
          enable = true;
          enableGuiTools = true;
          manageSway = true;
        };

        packages.x86_64-linux.noX = homePackage "x86_64-linux" {
          enable = true;
        };

        packages.aarch64-darwin.default = mkHomePackage self.homeConfigurations.macbook;

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
                ./home
              ];

              home.stateVersion = "22.05";

              myenv = {
                enable = true;
                enableGuiTools = config.services.xserver.enable;
                manageSway = config.programs.sway.enable;
                managePlasma = config.services.xserver.desktopManager.plasma5.enable;
              };
            };
          };
        };

        homeConfigurations = {
          macbook = home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.aarch64-darwin;

            modules = [
              plasma-manager.homeManagerModules.plasma-manager
              ./home

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
              })
            ];
          };
        };

        overlays.default = import ./overlay.nix inputs;
      }
    );
}
