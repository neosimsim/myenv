{ pkgs ? import <nixpkgs> { } }:
pkgs.buildGoModule {
  name = "go-scripts";
  CGO_ENABLED = "0";

  src = ./.;

  vendorSha256 = "sha256-xEKB+i6bbkdYZMsH5jHLUacff6aGam1Pm0JQUw9IuZY=";
}
