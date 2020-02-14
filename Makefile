.POSIX:

PREFIX=$(HOME)

install: install-noX
	. $(HOME)/.profile && $(MAKE) PREFIX=$(PREFIX) -C tools all

install-noX:
	$(MAKE) -C dotfiles install
	. $(HOME)/.profile && $(MAKE) PREFIX=$(PREFIX) -C scripts install-posix
	. $(HOME)/.profile && $(MAKE) PREFIX=$(PREFIX) -C tools noX
	. $(HOME)/.profile && $(MAKE) PREFIX=$(PREFIX) -C scripts install

