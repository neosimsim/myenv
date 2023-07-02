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

      testOptions = {
        x86_64-linux = {
          withPlasma5 = {
            enable = true;
            enableGuiTools = true;
            managePlasma5 = true;
          };
          withXMonad = {
            enable = true;
            enableGuiTools = true;
            manageXmonad = true;
          };
          withSway = {
            enable = true;
            enableGuiTools = true;
            manageSway = true;
          };
          noX = {
            enable = true;
          };
        };
        aarch64-darwin = {
          default = {
            enable = true;
            enableGuiTools = true;
          };
        };
      };

    in

    with flake-utils.lib; eachSystem [ system.x86_64-linux system.aarch64-darwin ]
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

        packages = nixpkgs.lib.mapAttrs
          (name: myenv:
            let
              homeConfig = home-manager.lib.homeManagerConfiguration {
                pkgs = nixpkgs.legacyPackages.${system};

                modules = [
                  plasma-manager.homeManagerModules.plasma-manager
                  ./nix/home

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
            with homeConfig.config;
            home.path // {
              inherit home-files;

              pkgs = setByName home.packages;
            }
          )
          testOptions.${system};

        checks = self.packages.${system} //
          (with nixpkgs.lib; mapAttrs'
            (name: pkgs: nameValuePair ("${name}-home-files") (pkgs.home-files))
            self.packages.${system}) // {
          # packages I added in overlay but not in home.packages:
          ma = pkgs.ma;
        } // nixpkgs.lib.optionalAttrs (system == flake-utils.lib.system.x86_64-linux) (
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
            checkWithPlasma5 = pkgs.runCommand "test-myenv-with-plasma5"
              rec {
                path = self.packages.x86_64-linux.withPlasma5;
                homeFiles = path.home-files;
              } ''
              ${checkPresent} $path/bin/emacs
              ${checkPresent} $path/bin/fm
              ${checkPresent} $path/bin/do-the-thing
              ${checkPresent} $path/bin/firefox
              ${checkPresent} $path/bin/chromium
              ${checkPresent} $path/bin/Afmt
              ${checkMissing} $path/bin/xmonad
              ${checkMissing} $path/bin/sway

              ${checkPresent} $homeFiles/.config/git/config
              ${checkPresent} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.mozilla/firefox/default/user.js
              ${checkPresentDir} $homeFiles/.config/chromium
              ${checkPresent} $homeFiles/lib/plumbing
              ${checkMissing} $homeFiles/.config/xmobar/xmobar.hs
              ${checkMissing} $homeFiles/.config/xmobar/xmobar

              echo successful >$out
            '';

            checkWithXmonad = pkgs.runCommand "test-myenv-with-xmonad"
              rec {
                path = self.packages.x86_64-linux.withXMonad;
                homeFiles = path.home-files;
              } ''
              ${checkPresent} $path/bin/xmonad
              ${checkMissing} $path/bin/sway

              ${checkPresent} $homeFiles/.config/xmobar/xmobar.hs
              ${checkPresent} $homeFiles/.config/xmobar/xmobar
              ${checkPresent} $homeFiles/.Xresources
              ${checkPresent} $homeFiles/.Xmodmap

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
              ${checkMissing} $path/bin/xmonad
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
              ${checkMissing} $path/bin/xmonad
              ${checkMissing} $path/bin/firefox
              ${checkMissing} $path/bin/chromium

              ${checkPresent} $homeFiles/.config/git/config
              ${checkMissing} $homeFiles/.Xresources
              ${checkMissing} $homeFiles/.config/sway/config
              ${checkMissing} $homeFiles/.mozilla/firefox/default/user.js
              ${checkMissing} $homeFiles/.config/chromium

              echo successful >$out
            '';
          }
        );
      }) // {
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
              ./nix/home
            ];

            home.stateVersion = "22.05";

            myenv = {
              enable = true;
              enableGuiTools = config.services.xserver.enable;
              manageSway = config.programs.sway.enable;
              managePlasma5 = config.services.xserver.desktopManager.plasma5.enable;
            };
          };
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
      };

      overlays.default = import ./nix/overlay.nix inputs;
    };
}
