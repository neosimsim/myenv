{
  description = "NeoSimSim: my-packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:neosimsim/home-manager/fish-extra-outputs-to-install-completions";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur.url = "github:nix-community/NUR";

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
    , nur
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
        packagesWithXServer = import ./nix/packages.nix { inherit pkgs; useXServer = true; };
        packagesWithSway = import ./nix/packages.nix { inherit pkgs; useSway = true; };
      };

      devShells.x86_64-linux.neosimsim-shell = pkgs.haskellPackages.shellFor {
        packages = p: with p; [ neosimsim-shell ];
      };

      nixosModules.default = { config, ... }: {

        imports = [
          home-manager.nixosModules.home-manager
        ];

        nixpkgs.overlays = [
          self.overlays.default
          self.inputs.nur.overlay
        ];

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;

          users.neosimsim = { ... }: {
            imports = [ (import ./nix/home.nix) ];

            myenv = {
              enable = true;
              useXServer = config.services.xserver.enable;
              useSway = config.programs.sway.enable;
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
            })
            self.nixosModules.default
          ];
        };
      };

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
          packagesWithoutGui = self.packages.x86_64-linux.packagesWithoutGui;
          packagesWithGui = self.packages.x86_64-linux.packagesWithGui;

          # disabled packages
          ma = self.packages.x86_64-linux.packagesWithGui.ma;

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
