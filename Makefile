.POSIX:

install: install-core

# use a new login shell to ensure installed dotfiles are sourced

install-gui: install-core install-gui-min

# gui setup (for non nix environments)
install-gui-min: install-min
	$(SHELL) -l -c '$(MAKE) -C dotfiles install-gui'
	$(SHELL) -l -c '$(MAKE) -C tools install-gui'

# core setup (for non nix environments)
install-core: install-min
	$(SHELL) -l -c '$(MAKE) -C scripts install'
	$(SHELL) -l -c '$(MAKE) -C texfiles install'

install-min:
	$(MAKE) -C dotfiles install
	$(SHELL) -l -c '$(MAKE) -C tools install'
	$(SHELL) -l -c '$(MAKE) -C aliases install'
	$(SHELL) -l -c gen-vis-uni

uninstall:
	$(SHELL) -l -c '$(MAKE) -C dotfiles uninstall'
	$(SHELL) -l -c '$(MAKE) -C texfiles uninstall'
	$(SHELL) -l -c '$(MAKE) -C scripts uninstall'
	$(SHELL) -l -c '$(MAKE) -C aliases uninstall'

reload-core:
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload-core'

reload-gui:
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload-gui'

test-core:
	$(SHELL) -l -c '$(MAKE) -C scripts test'
	$(SHELL) -l -c '$(MAKE) -C texfiles pdf'
	$(SHELL) -l -c '$(MAKE) -C tests test-core'

test-gui:
	$(SHELL) -l -c '$(MAKE) -C tests test-gui'
