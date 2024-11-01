{ pkgs, config, lib, ... }: {
  options = {
    myenv.chromium = {
      enable = lib.mkEnableOption ''
        Install and configure Chromium.
      '';
    };
  };
  config = lib.mkIf config.myenv.chromium.enable {
    programs.chromium = rec {
      enable = true;

      package = pkgs.ungoogled-chromium;
      # https://github.com/nix-community/home-manager/issues/2216
      extensions =
        let
          createChromiumExtensionFor = browserVersion: { id, sha256, version }:
            {
              inherit id;
              crxPath = builtins.fetchurl {
                url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
                name = "${id}.crx";
                inherit sha256;
              };
              inherit version;
            };
          createChromiumExtension = createChromiumExtensionFor (lib.versions.major package.version);
        in
        [
          (createChromiumExtension {
            # https://chrome.google.com/webstore/detail/ublock-origin/cjpalhdlnbpafiamejdnhcphjbkeiagm
            id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
            version = "1.57.0";
            sha256 = "sha256:0fsygwn7rff79405fr96j89i9fx7w8dl3ix9045ymgm0mf3747pd";
          })
          (createChromiumExtension {
            # https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb
            id = "dbepggeogbaibhgnhhndojpepiihcmeb";
            version = "2.1.2";
            sha256 = "sha256:0m8xski05w2r8igj675sxrlkzxlrl59j3a7m0r6c8pwcvka0r88d";
          })
        ];
    };
  };
}
