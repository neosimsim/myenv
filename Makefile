.POSIX:

install: install-core

install-gui: install-core

install-core:
	$(MAKE) -C scripts install
	$(MAKE) -C texfiles install
	cd golang && go install ./...
	cd haskell && cabal update && cabal install

uninstall:
	$(MAKE) -C texfiles uninstall
	$(MAKE) -C scripts uninstall

test: test-core

test-core:
	$(MAKE) -C texfiles pdf
	$(MAKE) -C tests test-core

test-gui: test-core
	$(MAKE) -C tests test-gui
