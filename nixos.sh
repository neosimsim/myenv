#!/bin/sh

set -e

export NIXPKGS_ALLOW_UNFREE=1

if [ -n "$DISPLAY" ]; then
	nix-env --arg enableGui true -if .
else
	nix-env -if .
fi

make install-nixos
make reload-core
make test-nixos

if [ -n "$DISPLAY" ]; then
	make reload-gui
	make test-gui
fi
