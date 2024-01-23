{ pkgs ? import <nixpkgs> { } }:
pkgs.buildGoModule {
  name = "utils";
  CGO_ENABLED = "0";

  src = ./.;

  vendorHash = "sha256-xEKB+i6bbkdYZMsH5jHLUacff6aGam1Pm0JQUw9IuZY=";
}
