inputs: final: prev: {

  haskellPackages = with prev;
    (haskellPackages.override {
      overrides = finalHs: prevHs: {
        # Using callCabal2nix, which is used by packageSourceOverrides,
        # breaks `nix show` and `nix check`.
        # https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
        hconv = finalHs.callPackage ./hconv.nix { };
        neosimsim-shell = finalHs.callPackage ../tools/shell { };
        hookmark = (inputs.hookmark.overlays.default final prev).haskellPackages.hookmark;
      };
    });

  editinacme = final.buildGoModule {
    name = "editinacme";

    src = inputs.plan9fansGo;

    vendorSha256 = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

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

    vendorSha256 = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

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

    vendorSha256 = "sha256-qwoYzbfyek/472x24srND/9qX2UsiKzJleLV7cFDVsY=";

    buildPhase = ''
      go install 9fans.net/go/acme/Watch
    '';

    meta = with inputs.nixpkgs.lib; {
      homepage = "https://github.com/9fans/go";
      license = licenses.mit;
      platforms = platforms.linux ++ platforms.darwin;
    };
  };

  goimports = final.buildGoModule {
    name = "goimports";

    src = inputs.goTools;

    vendorSha256 = "sha256-vnXMBx0iaXYawmLf/iAncjkkKRP+34GoMFRddjPWlJc=";

    proxyVendor = true;

    buildPhase = ''
      go install golang.org/x/tools/cmd/goimports
    '';

    meta = with inputs.nixpkgs.lib; {
      homepage = "https://pkg.go.dev/golang.org/x/tools/cmd/goimports";
      license = licenses.bsd3;
      platforms = platforms.linux ++ platforms.darwin;
    };
  };

  gosec = final.buildGoModule {
    name = "gosec";

    src = inputs.gosec;

    #vendorSha256 = final.lib.fakeHash;
    vendorSha256 = "sha256-sxlymgPjfduVvR6Iq1lj3FMY5XXzmezdWF0JoQctPzM=";

    buildPhase = ''
      go install github.com/securego/gosec/v2/cmd/gosec
    '';

    meta = with inputs.nixpkgs.lib; {
      homepage = "https://github.com/securego/gosec";
      license = licenses.apsl20;
      platforms = platforms.linux ++ platforms.darwin;
    };
  };

  passage = final.stdenv.mkDerivation {
    name = "passage";
    src = inputs.passage;

    buildPhase = ''true'';

    nativeBuildInputs = with final; [ makeWrapper installShellFiles ];
    installPhase = ''
      make DESTDIR= PREFIX=$out install

      wrapProgram $out/bin/passage --prefix PATH : ${with final; lib.makeBinPath [ age tree xclip ]}

      installShellCompletion --cmd passage \
        --bash src/completion/pass.bash-completion \
        --fish src/completion/pass.fish-completion \
        --zsh src/completion/pass.zsh-completion
    '';
  };

  ma = final.stdenv.mkDerivation rec {
    pname = "ma";
    version = "11_2019-03-16";
    src = fetchTarball {
      url = "http://www.call-with-current-continuation.org/ma/ma.tar.gz";
      sha256 = "0g0lqijkwg5p0586spli2jd1yh0im0ma4fnhkf8mizhyrsj7ga2s";
    };
    cmds = [
      "awd"
      "B"
      "ma"
      "ma-eval"
      "plumb"
      "pty"
      "win"
    ];

    buildInputs = [ final.tk ];
    buildPhase = "./build";
    installPhase = ''
      mkdir -p $out/bin
      cp ${final.lib.concatStringsSep " " cmds} $out/bin
    '';
  };

}
