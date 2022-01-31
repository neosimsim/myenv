.POSIX:

install: install-core

# use a new login shell to ensure installed dotfiles are sourced

install-gui:
	$(SHELL) -l -c '$(MAKE) -C dotfiles install-gui'
	$(SHELL) -l -c '$(MAKE) -C tools install-gui'

# stuff installed as part of default.nix
install-core: install-nixos
	$(MAKE) -C dotfiles install
	$(SHELL) -l -c '$(MAKE) -C scripts install'
	$(SHELL) -l -c '$(MAKE) -C texfiles install'

# stuff not installed as part of default.nix
install-nixos:
	$(SHELL) -l -c '$(MAKE) -C tools install'
	$(SHELL) -l -c '$(MAKE) -C aliases install'

uninstall:
	$(SHELL) -l -c '$(MAKE) -C dotfiles uninstall'
	$(SHELL) -l -c '$(MAKE) -C texfiles uninstall'
	$(SHELL) -l -c '$(MAKE) -C scripts uninstall'
	$(SHELL) -l -c '$(MAKE) -C aliases uninstall'

reload-core:
	$(SHELL) -l -c gen-vis-uni
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload-core'

reload-gui:
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload-gui'

test: test-core

test-core:
	$(SHELL) -l -c '$(MAKE) -C texfiles pdf'
	$(SHELL) -l -c '$(MAKE) -C tests test-core'

test-gui:
	$(SHELL) -l -c '$(MAKE) -C tests test-gui'
