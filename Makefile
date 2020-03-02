.POSIX:

install: install-noX
	. $(HOME)/.profile && $(MAKE) PREFIX=$(PREFIX) -C tools all

install-noX:
	$(MAKE) -C dotfiles install
	. $(HOME)/.profile && $(MAKE) -C scripts install-posix
	. $(HOME)/.profile && $(MAKE) -C tools noX
	. $(HOME)/.profile && $(MAKE) -C scripts install

