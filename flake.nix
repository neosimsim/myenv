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

    eglot-x = {
      url = "github:nemethf/eglot-x";
      flake = false;
    };

    combobulate = {
      url = "github:mickeynp/combobulate";
      flake = false;
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
  };

  outputs =
    { self
    , combobulate
    , emacs-overlay
    , eglot-x
    , flake-utils
    , home-manager
    , nixpkgs
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
        mapAttrs'
        nameValuePair
        listToAttrs
        ;

      inherit (flake-utils.lib)
        system
        eachSystem
        ;

      setByName = list: listToAttrs (map
        (x: {
          name = (replaceStrings [ "." ] [ "-" ] (getName x));
          value = x;
        })
        list);

    in

    eachSystem [ system.x86_64-linux system.aarch64-darwin ]
      (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        checks =
          self.packages.${system}
          // (mapAttrs'
            (name: pkgs: nameValuePair "${name}-home-files" pkgs.home-files)
            self.packages.${system})
          // nixpkgs.lib.optionalAttrs (system == flake-utils.lib.system.x86_64-linux) (
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
              checkWithPlasma = pkgs.runCommand "test-myenv-with-plasma"
                rec {
                  path = self.packages.x86_64-linux.withPlasma;
                  homeFiles = path.home-files;
                } ''
                ${checkPresent} $path/bin/emacs
                ${checkPresent} $path/bin/fm
                ${checkPresent} $path/bin/dtt
                ${checkPresent} $path/bin/firefox-esr
                ${checkPresent} $path/bin/chromium
                ${checkPresent} $path/bin/Afmt
                ${checkPresent} $path/bin/ghc
                ${checkPresent} $path/bin/haskell-language-server-wrapper

                ${checkPresent} $homeFiles/.config/git/config
                ${checkPresent} $homeFiles/.Xresources
                ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
                ${checkPresent} $homeFiles/.ghci
                ${checkPresentDir} $homeFiles/.config/chromium
                ${checkPresent} $homeFiles/lib/plumbing

                echo successful >$out
              '';

              checkWithPlasmaWayland = pkgs.runCommand "test-myenv-with-plasma-wayland"
                rec {
                  path = self.packages.x86_64-linux.withPlasmaWayland;
                  homeFiles = path.home-files;
                } ''
                ${checkPresent} $path/bin/emacs
                ${checkPresent} $path/bin/fm
                ${checkPresent} $path/bin/dtt
                ${checkPresent} $path/bin/firefox-esr
                ${checkPresent} $path/bin/chromium
                ${checkPresent} $path/bin/Afmt
                ${checkPresent} $path/bin/ghc
                ${checkPresent} $path/bin/wl-copy
                ${checkPresent} $path/bin/haskell-language-server-wrapper

                ${checkPresent} $homeFiles/.config/git/config
                ${checkMissing} $homeFiles/.Xresources
                ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
                ${checkPresent} $homeFiles/.ghci
                ${checkPresentDir} $homeFiles/.config/chromium
                ${checkPresent} $homeFiles/lib/plumbing

                echo successful >$out
              '';

              checkNoX = pkgs.runCommand "test-myenv-noX"
                rec {
                  path = self.packages.x86_64-linux.noX;
                  homeFiles = path.home-files;
                } ''
                ${checkPresent} $path/bin/fm
                ${checkPresent} $path/bin/dtt
                ${checkMissing} $path/bin/firefox-esr
                ${checkMissing} $path/bin/chromium
                ${checkMissing} $path/bin/ghc

                ${checkPresent} $homeFiles/.config/git/config
                ${checkMissing} $homeFiles/.Xresources
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

              extraSpecialArgs = { inherit inputs; };

              modules = [
                ./home

                ({ pkgs, config, ... }: {
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
          coreutils.enable = true;
          plasma.enable = true;
          xserver.enable = true;
          ghc.enable = true;
          plan9port.enable = true;
          chromium.enable = true;
          firefox.enable = true;
          emacs.enable = true;
          go.enable = true;
        };

        packages.x86_64-linux.withPlasmaWayland = homePackage "x86_64-linux" {
          coreutils.enable = true;
          plasma.enable = true;
          wayland.enable = true;
          ghc.enable = true;
          plan9port.enable = true;
          chromium.enable = true;
          firefox.enable = true;
          emacs.enable = true;
          go.enable = true;
        };

        packages.x86_64-linux.noX = homePackage "x86_64-linux" {
          coreutils.enable = true;
          plan9port.enable = true;
          emacs.enable = true;
        };

        packages.aarch64-darwin.default = mkHomePackage self.homeConfigurations.neosimsim;

        nixosModules.default = { config, ... }: {

          imports = [
            home-manager.nixosModules.home-manager
          ];

          home-manager = {
            useGlobalPkgs = false;
            useUserPackages = true;
            extraSpecialArgs = { inherit inputs; };

            users.neosimsim = { ... }: {
              imports = [
                ./home
              ];
            };
          };
        };

        homeConfigurations = {
          neosimsim = home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.aarch64-darwin;

            extraSpecialArgs = { inherit inputs; };

            modules = [
              ./home

              ({ pkgs, config, ... }: {
                home = {
                  stateVersion = "22.05";
                  username = "neosimsim";
                  homeDirectory = "/Users/neosimsim";
                };

                myenv.coreutils.enable = true;
              })
            ];
          };
        };

        overlays.default = import ./overlay.nix inputs;
      }
    );
}
