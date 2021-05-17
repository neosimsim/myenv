#!/bin/sh

set -e

export NIXPKGS_ALLOW_UNFREE=1

if [ -n "$DISPLAY" ]; then
	nix-env --arg enableGui true -if .
	make install-gui-min
	make reload-gui
else
	nix-env -if .
	make install-min
	make reload-core
fi