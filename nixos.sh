#!/bin/sh

set -e

packages=my-packages
[ -n "$DISPLAY" ] && packages=my-gui-packages

if nix profile list | grep -q $packages
then
        nix profile upgrade packages.x86_64-linux.$packages
else
	nix profile install .#$packages
fi

make install-nixos
make reload-core
make test-nixos

if [ -n "$DISPLAY" ]; then
	make reload-gui
	make test-gui
fi
