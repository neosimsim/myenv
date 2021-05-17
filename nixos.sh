#!/bin/sh

set -e

if [ -n "$DISPLAY" ]; then
	nix-env --arg enableGui true -if .
	make install-gui-min
else
	nix-env -if .
	make install-min
fi