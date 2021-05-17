.POSIX:

default: install-core
install: install-core

# use a new login shell to ensure installed dotfiles are sourced

install-gui: install-core
	$(SHELL) -l -c 'nix-env --arg enableGui true -if .'
	$(SHELL) -l -c '$(MAKE) -C dotfiles install-gui'
	$(SHELL) -l -c '$(MAKE) -C tools install-gui'

install-core:
	$(MAKE) -C dotfiles install-core
	$(SHELL) -l -c '$(MAKE) -C tools install-core'
	$(SHELL) -l -c '$(MAKE) -C aliases install'
	$(SHELL) -l -c 'nix-env -if .'
	$(SHELL) -l -c gen-vis-uni

uninstall:
	$(MAKE) -C dotfiles uninstall
	$(MAKE) -C texfiles uninstall
	$(MAKE) -C scripts uninstall
	$(MAKE) -C aliases uninstall

reload-core:
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload'

reload-gui:
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload'

test-core:
	$(SHELL) -l -c '$(MAKE) -C scripts test'
	$(SHELL) -l -c '$(MAKE) -C texfiles pdf'
	$(SHELL) -l -c '$(MAKE) -C tests test-core'

test-gui:
	$(SHELL) -l -c '$(MAKE) -C tests test-gui'
