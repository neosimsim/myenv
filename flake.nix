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

      overlays.default = final: prev: {

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

          vendorSha256 = "sha256-vIRXyekU7DNWLg/sQplchGfW0en0OMLKdiuQKLxf20w=";

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

          vendorSha256 = "sha256-3ZGzVGKwnNab8wUn0fRepl4FDo43MAqNAO3zijH90/0=";

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
