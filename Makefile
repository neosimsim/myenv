.POSIX:

PREFIX=$(HOME)

install:
	$(MAKE) -C dotfiles install
	$(MAKE) PREFIX=$(PREFIX) -C scripts install-posix
	$(MAKE) PREFIX=$(PREFIX) -C tools install
	$(MAKE) PREFIX=$(PREFIX) -C scripts install

