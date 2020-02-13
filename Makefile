.POSIX:

PREFIX=$(HOME)

install:
	$(MAKE) -C cabal-extras INSTALL_FLAGS="--installdir $(PREFIX)/bin --install-method copy" install-cabal-env
	$(MAKE) -C packages install
