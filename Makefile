.POSIX:

default: install-core
install: install-core

install-gui: install-core
	$(SHELL) -l -c '$(MAKE) -C dotfiles install-gui'
	$(SHELL) -l -c 'nix-env -f "<nixpkgs>" -iA myGuiPackages'
	$(SHELL) -l -c '$(MAKE) -C tools install-gui'

install-core:
	$(MAKE) -C dotfiles install-core
	$(MAKE) -C texfiles install
	# Some scripts are required by tools BUT tools can only use
	# posix compatible scripts which don't require additional programs like
	# GHC.
	$(SHELL) -l -c '$(MAKE) -C scripts install-posix'
	$(SHELL) -l -c '$(MAKE) -C aliases install'
	$(SHELL) -l -c '$(MAKE) -C tools install-core'
	$(SHELL) -l -c 'nix-env -f "<nixpkgs>" -iA myPackages'
	# We can now install every other script.
	$(SHELL) -l -c '$(MAKE) -C scripts install'
	$(SHELL) -l -c gen-vim-uni

uninstall:
	$(MAKE) -C dotfiles uninstall
	$(MAKE) -C texfiles uninstall
	$(MAKE) -C scripts uninstall
	$(MAKE) -C aliases uninstall

test-core:
	$(SHELL) -l -c '$(MAKE) -C scripts test'
	$(SHELL) -l -c '$(MAKE) -C texfiles pdf'
	$(SHELL) -l -c '$(MAKE) -C tests test-core'

test-gui:
	$(SHELL) -l -c '$(MAKE) -C tests test-gui'
