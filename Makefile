.POSIX:

install: install-core

# use a new login shell to ensure installed dotfiles are sourced
# TODO can this be done defining SHELL and the beginning of the file? eg:
# SHELL = $(SHELL) -l

install-gui: install-core
	$(SHELL) -l -c '$(MAKE) -C dotfiles install-gui'
	$(SHELL) -l -c '$(MAKE) -C tools install-gui'

install-core:
	$(MAKE) -C dotfiles install
	$(SHELL) -l -c '$(MAKE) -C scripts install'
	$(SHELL) -l -c '$(MAKE) -C texfiles install'
	$(SHELL) -l -c 'cd golang && go install ./...'
	$(SHELL) -l -c 'cd haskell && cabal update && cabal install'
	$(SHELL) -l -c '$(MAKE) -C tools install'

uninstall:
	$(SHELL) -l -c '$(MAKE) -C dotfiles uninstall'
	$(SHELL) -l -c '$(MAKE) -C texfiles uninstall'
	$(SHELL) -l -c '$(MAKE) -C scripts uninstall'

reload-core:
	$(SHELL) -l -c gen-vis-uni
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload-core'

reload-gui:
	$(SHELL) -l -c '$(MAKE) -C dotfiles reload-gui'

test: test-core

test-core:
	$(SHELL) -l -c '$(MAKE) -C texfiles pdf'
	$(SHELL) -l -c '$(MAKE) -C tests test-core'

test-gui: test-core
	$(SHELL) -l -c '$(MAKE) -C tests test-gui'
