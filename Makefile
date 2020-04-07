.POSIX:

install: install-noX
	. $(HOME)/.profile && $(MAKE) -C tools X

install-noX:
	$(MAKE) -C dotfiles install
	$(MAKE) -C texfiles install
	# Some scripts are required by tools (e. g. `g`) BUT tools can only use
	# posix compatible scripts which don't require additional programs like
	# GHC.
	. $(HOME)/.profile && $(MAKE) -C scripts install-posix
	. $(HOME)/.profile && $(MAKE) -C aliases install
	. $(HOME)/.profile && $(MAKE) -C tools noX
	# We can now install every other script.
	. $(HOME)/.profile && $(MAKE) -C scripts install

uninstall:
	$(MAKE) -C dotfiles uninstall
	$(MAKE) -C texfiles uninstall
	$(MAKE) -C scripts uninstall
	$(MAKE) -C aliases uninstall

test:
	. $(HOME)/.profile && $(MAKE) -C scripts test
	. $(HOME)/.profile && $(MAKE) -C texfiles pdf
	. $(HOME)/.profile && $(MAKE) -C tests all
