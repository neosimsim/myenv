{ pkgs, config, lib, ... }: with lib;
{
  programs.chromium = rec {
    enable =
      config.myenv.enableGuiTools
      && pkgs.stdenv.isLinux;

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
          version = "1.43.0";
          sha256 = "sha256:0izlm20b0bpjr98rw63c42zb04ky1mclg0da2xmj0kw3qichnpvg";
        })
        (createChromiumExtension {
          # https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb
          id = "dbepggeogbaibhgnhhndojpepiihcmeb";
          version = "1.67";
          sha256 = "sha256:097axwrhn8g26kp25w86x71khaqcw3nb0ras9ndwqvdw3bpgkcd8";
        })
      ];
  };
}
