{
  packageOverrides = pkgs: with pkgs; rec {
    myProfile = writeText "my-profile" ''
      export PATH=$HOME/.nix-profile/bin:$PATH
      export MANPATH=$HOME/.nix-profile/share/man:$MANPATH
    '';
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        (runCommand "profile" {} ''
          mkdir -p $out/etc/profile.d
          cp ${myProfile} $out/etc/profile.d/my-profile.sh
        '')
        jq
        signal-desktop
        wire-desktop
        haskellPackages.pandoc
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc"];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
}

