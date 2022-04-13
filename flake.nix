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
    }@inputs:
    let pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlays.default ];
    };
    in
    {
      packages.x86_64-linux = {
        packagesWithoutGui = import ./nix/packages.nix { inherit pkgs; enableGui = false; };
        packagesWithGui = import ./nix/packages.nix { inherit pkgs; enableGui = true; };
      };


      devShells.x86_64-linux.neosimsim-shell = pkgs.haskellPackages.shellFor {
        packages = p: with p; [ neosimsim-shell ];
      };

      nixosModule = { config, ... }: {

        imports = [
          home-manager.nixosModules.home-manager
        ];

        nixpkgs.overlays = [
          self.overlays.default
        ];

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;

          users.neosimsim = { ... }: {
            imports = [ (import ./nix/home.nix) ];

            myenv = {
              enable = true;
              enableGui = config.services.xserver.enable || config.programs.sway.enable;
            };
          };
        };

      };

      overlays.default = import ./nix/overlay.nix inputs;

      nixosConfigurations = {
        withoutGui = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ({ ... }: {
              boot.isContainer = true;
              users.users.neosimsim.isNormalUser = true;
            })
            self.nixosModule
          ];
        };

        withGui = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ({ ... }: {
              boot.isContainer = true;
              services.xserver.enable = true;
              users.users.neosimsim.isNormalUser = true;
            })
            self.nixosModule
          ];
        };
      };

      checks.x86_64-linux =
        let
          pkgs = import nixpkgs {
            system = "x86_64-linux";
          };
          checkInstalled = pkgs.writeShellScript "checkInstalled" ''
            if ! [ -x $1 ]; then
              echo missing $1 >&2
              exit 1
            fi
          '';
          checkUninstalled = pkgs.writeShellScript "checkUninstalled" ''
            if [ -x $1 ]; then
              echo unexpected $1 >&2
              exit 1
            fi
          '';
        in
        {
          packagesWithoutGui = self.packages.x86_64-linux.packagesWithoutGui;
          packagesWithGui = self.packages.x86_64-linux.packagesWithGui;

          # disabled packages
          ma = self.packages.x86_64-linux.packagesWithGui.ma;

          nixosWithGui = pkgs.runCommand "test-myenv-with-gui"
            {
              nixRoot = self.nixosConfigurations.withGui.config.system.build.toplevel;
            } ''
            ${checkInstalled} $nixRoot/etc/profiles/per-user/neosimsim/bin/Afmt
            ${checkInstalled} $nixRoot/etc/profiles/per-user/neosimsim/bin/xmonad

            echo successful >$out
          '';

          nixosWithoutGui = pkgs.runCommand "test-myenv-without-gui"
            {
              nixRoot = self.nixosConfigurations.withoutGui.config.system.build.toplevel;
            } ''
            ${checkInstalled} $nixRoot/etc/profiles/per-user/neosimsim/bin/emacs
            ${checkInstalled} $nixRoot/etc/profiles/per-user/neosimsim/bin/fm
            ${checkInstalled} $nixRoot/etc/profiles/per-user/neosimsim/bin/o
            ${checkUninstalled} $nixRoot/etc/profiles/per-user/neosimsim/bin/xmonad

            echo successful >$out
          '';
        };
    };
}
