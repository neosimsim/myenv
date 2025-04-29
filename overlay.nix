final: prev: {
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

  gotools = prev.gotools.overrideAttrs (oldAttrs: {
    excludedPackages = [
      # conflict with my scripts bundle
      "bundle"
    ];
  });
}
