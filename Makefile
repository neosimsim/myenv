.POSIX:

PREFIX=$(HOME)

install: install-noX
	$(MAKE) PREFIX=$(PREFIX) -C tools all

install-noX:
	$(MAKE) -C dotfiles install
	$(MAKE) PREFIX=$(PREFIX) -C scripts install-posix
	$(MAKE) PREFIX=$(PREFIX) -C tools noX
	$(MAKE) PREFIX=$(PREFIX) -C scripts install

