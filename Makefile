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
	$(SHELL) -l -c 'nix-env -f scripts -i'
	$(SHELL) -l -c '$(MAKE) -C aliases install'
	$(SHELL) -l -c '$(MAKE) -C tools install-core'
	$(SHELL) -l -c 'nix-env -f "<nixpkgs>" -iA myPackages'
	$(SHELL) -l -c gen-vis-uni

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
