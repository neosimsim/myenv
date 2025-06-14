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

    ob-nushell =
      {
        url = "github:b3tchi/ob-nushell";
        flake = false;
      };

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    plan9fansGo = {
      url = "github:9fans/go";
      flake = false;
    };
  };

  outputs =
    { self
    , combobulate
    , ob-nushell
    , emacs-overlay
    , eglot-x
    , flake-utils
    , home-manager
    , nixpkgs
    , plan9fansGo
    , plasma-manager
    }:
    let

      inputsModule = {
        imports = [
          plasma-manager.homeManagerModules.plasma-manager
        ];

        config = {
          nixpkgs.overlays = [
            emacs-overlay.overlay
            self.overlays.default

            (final: prev: {
              emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope (emacsFinal: emacsPrev: {
                eglot-x = emacsFinal.trivialBuild {
                  pname = "eglot-x";
                  version = "0.6";
                  src = eglot-x;
                };
                combobulate = emacsFinal.trivialBuild {
                  pname = "combobulate";
                  version = "0.1";
                  src = combobulate;
                };
                ob-nushell = emacsFinal.trivialBuild {
                  pname = "ob-nushell";
                  version = "0.1";
                  src = ob-nushell;
                };
              });

              editinacme = prev.buildGoModule {
                name = "editinacme";

                src = plan9fansGo;

                vendorHash = "sha256-/pjqN11vZF+AX3OctaDAMb+s4W173bMWkjkNbUD14GA";

                buildPhase = ''
                  go install 9fans.net/go/acme/editinacme
                '';

                meta = with prev.lib; {
                  homepage = "https://github.com/9fans/go";
                  license = licenses.mit;
                  platforms = platforms.linux ++ platforms.darwin;
                };
              };

              acmego = prev.buildGoModule {
                name = "acmego";

                src = plan9fansGo;

                vendorHash = "sha256-/pjqN11vZF+AX3OctaDAMb+s4W173bMWkjkNbUD14GA=";

                buildPhase = ''
                  go install 9fans.net/go/acme/acmego
                '';

                meta = with prev.lib; {
                  homepage = "https://github.com/9fans/go";
                  license = licenses.mit;
                  platforms = platforms.linux ++ platforms.darwin;
                };
              };

              Watch = prev.buildGoModule {
                name = "Watch";

                src = plan9fansGo;

                vendorHash = "sha256-/pjqN11vZF+AX3OctaDAMb+s4W173bMWkjkNbUD14GA=";

                buildPhase = ''
                  go install 9fans.net/go/acme/Watch
                '';

                meta = with prev.lib; {
                  homepage = "https://github.com/9fans/go";
                  license = licenses.mit;
                  platforms = platforms.linux ++ platforms.darwin;
                };
              };

            })
          ];
        };
      };

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
              checkWithPlasma = pkgs.runCommand "test-myenv-with-plasma-wayland"
                rec {
                  path = self.packages.x86_64-linux.withPlasma;
                  homeFiles = path.home-files;
                } ''
                ${checkPresent} $path/bin/emacs
                ${checkPresent} $path/bin/fm
                ${checkPresent} $path/bin/dtt
                ${checkPresent} $path/bin/Afmt
                ${checkPresent} $path/bin/ghc
                ${checkPresent} $path/bin/wl-copy
                ${checkPresent} $path/bin/haskell-language-server-wrapper

                ${checkPresent} $homeFiles/.config/git/config
                ${checkMissing} $homeFiles/.Xresources
                ${checkPresent} $homeFiles/.ghci
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
                ${checkMissing} $path/bin/ghc

                ${checkPresent} $homeFiles/.config/git/config
                ${checkMissing} $homeFiles/.Xresources

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
                inputsModule
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
          wayland.enable = true;
          ghc.enable = true;
          plan9port.enable = true;
          emacs.enable = true;
          go.enable = true;
          texlive.enable = true;
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

            users.neosimsim = { ... }: {
              imports = [
                inputsModule
                ./home
              ];
            };
          };
        };

        homeConfigurations = {
          neosimsim = home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.aarch64-darwin;

            modules = [
              inputsModule
              ./home

              ({ pkgs, config, ... }: {
                programs.zsh.enable = true;
                programs.zsh.initExtra = ''
                  # Nix
                  # macOS overwrite /etc/zshrc all the time, so we set it here also.
                  if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
                    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
                  fi
                  # End Nix
                '';

                home = {
                  stateVersion = "22.05";
                  username = "neosimsim";
                  homeDirectory = "/Users/neosimsim";

                  packages = with pkgs; [
                    cgoban
                  ];

                  sessionPath = [
                    "/Applications/Racket v8.15/bin"
                  ];
                };

                myenv.coreutils.enable = true;
                myenv.emacs.enable = true;
              })
            ];
          };
        };

        overlays.default = import ./overlay.nix;
      }
    );
}
