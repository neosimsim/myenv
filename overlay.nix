inputs: final: prev: {

  haskellPackages = with prev;
    (haskellPackages.override {
      overrides = finalHs: prevHs: {
        # Using callCabal2nix, which is used by packageSourceOverrides,
        # breaks `nix show` and `nix check`.
        # https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
        hconv = finalHs.callPackage ./pkgs/hconv.nix { };
        hookmark = (inputs.hookmark.overlays.default final prev).haskellPackages.hookmark;
      };
    });

  utils-scripts = import ./pkgs/utils-scripts { pkgs = prev; };

  utils-go = import ./pkgs/utils-go { pkgs = prev; };

  utils-haskell = (import ./pkgs/utils-haskell { pkgs = prev; }).scripts;

  cabal-shell = prev.callPackage ./pkgs/cabal-shell { };

  texlive-full =
    let
      texfiles.pkgs = [ (import ./pkgs/texfiles { pkgs = prev; }) ];
    in
    prev.texlive.combine {
      inherit (prev.texlive) scheme-full;
      inherit texfiles;
    };

  editinacme = final.buildGoModule {
    name = "editinacme";

    src = inputs.plan9fansGo;

    vendorHash = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

    buildPhase = ''
      go install 9fans.net/go/acme/editinacme
    '';

    meta = with inputs.nixpkgs.lib; {
      homepage = "https://github.com/9fans/go";
      license = licenses.mit;
      platforms = platforms.linux ++ platforms.darwin;
    };
  };

  acmego = final.buildGoModule {
    name = "acmego";

    src = inputs.plan9fansGo;

    vendorHash = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

    buildPhase = ''
      go install 9fans.net/go/acme/acmego
    '';

    meta = with inputs.nixpkgs.lib; {
      homepage = "https://github.com/9fans/go";
      license = licenses.mit;
      platforms = platforms.linux ++ platforms.darwin;
    };
  };

  Watch = final.buildGoModule {
    name = "Watch";

    src = inputs.plan9fansGo;

    vendorHash = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

    buildPhase = ''
      go install 9fans.net/go/acme/Watch
    '';

    meta = with inputs.nixpkgs.lib; {
      homepage = "https://github.com/9fans/go";
      license = licenses.mit;
      platforms = platforms.linux ++ platforms.darwin;
    };
  };

  gotools = prev.gotools.overrideAttrs (oldAttrs: {
    excludedPackages = [
      # conflict with my scripts bundle
      "bundle"
    ];
  });
}
