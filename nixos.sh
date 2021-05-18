#!/bin/sh

set -e

export NIXPKGS_ALLOW_UNFREE=1
export NIX_PATH='nixpkgs=https://github.com/NixOS/nixpkgs/archive/bd017d34f4c914fc1842b9fd5b4fc6ca85ab61de.tar.gz'

if [ -n "$DISPLAY" ]; then
	nix-env --arg enableGui true -if .
	make install-gui-min
	make reload-gui
else
	nix-env -if .
	make install-min
	make reload-core
fi