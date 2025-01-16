{ pkgs ? import <nixpkgs> { } }:
pkgs.buildGoModule {
  name = "utils";
  env.CGO_ENABLED = "0";

  src = ./.;

  vendorHash = "sha256-8bs1W7Bsdi3cVlefSru46nu2qK/1yA0xev2HV6FUs6w=";
}
