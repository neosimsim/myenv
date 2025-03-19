inputs: final: prev: {
  utils-scripts = import ./pkgs/utils-scripts { pkgs = prev; };

  utils-go = import ./pkgs/utils-go { pkgs = prev; };

  utils-rust = prev.callPackage ./pkgs/utils-rust { };

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

    vendorHash = "sha256-/pjqN11vZF+AX3OctaDAMb+s4W173bMWkjkNbUD14GA";

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

    vendorHash = "sha256-/pjqN11vZF+AX3OctaDAMb+s4W173bMWkjkNbUD14GA=";

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

    vendorHash = "sha256-/pjqN11vZF+AX3OctaDAMb+s4W173bMWkjkNbUD14GA=";

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
