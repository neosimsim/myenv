.POSIX:

PREFIX=$(HOME)
CABAL_FLAGS=--installdir $(PREFIX)/bin --install-method copy
GHC_VERSION=8.6.5
GHC_CONFIG=

install:
	mkdir -p $(PREFIX)/bin
	cabal v2-update
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) cabal-install
	make -C cabal-extras CABAL_FLAGS="$(INSTALL_FLAGS)" install-cabal-env
	cabal v2-install $(CABAL_FLAGS) xmonad
	cabal v2-install $(CABAL_FLAGS) -f with_alsa -f with_xft xmobar
	cabal-env -n xmonad xmonad xmonad-contrib X11
	# Unable to install hfmt with haskell-src-exts 1.22
	# https://github.com/chrisdone/hindent/issues/562
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) --constraint 'haskell-src-exts < 1.22' hfmt hindent hlint stylish-haskell
	cabal v2-install $(CABAL_FLAGS) cabal-fmt
	cabal v2-install $(CABAL_FLAGS) pandoc pandoc-include-code
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) Agda
	g -v 8.6.5 cabal-env -n agda ieee text


ghcs:
	$(MAKE) GHC_VERSION=8.6.5 ghc
	$(MAKE) GHC_VERSION=8.8.2 ghc

ghc: $(HOME)/apps/ghc-$(GHC_VERSION)/bin/ghc
	ls $(HOME)/apps/ghc-$(GHC_VERSION)/bin/ghc
	mkdir -p $(HOME)/src
	cd $(HOME)/src && wget https://downloads.haskell.org/~ghc/$(GHC_VERSION)/ghc-$(GHC_VERSION)-src.tar.xz
	cd $(HOME)/src && tar xf ghc-$(GHC_VERSION)-src.tar.xz
	cd $(HOME)/src/ghc-$(GHC_VERSION) && ./configure --prefix=$(HOME)/apps/ghc-$(GHC_VERSION)
	mkdir -p $(HOME)/src
	cd $(HOME)/src && wget https://downloads.haskell.org/~ghc/$(GHC_VERSION)/ghc-$(GHC_VERSION)-src.tar.xz
	cd $(HOME)/src && tar xf ghc-$(GHC_VERSION)-src.tar.xz
	cd $(HOME)/src/ghc-$(GHC_VERSION) && ./configure --prefix=$(HOME)/apps/ghc-$(GHC_VERSION) $(GHC_CONFIG)
	cd $(HOME)/src/ghc-$(GHC_VERSION) && make -j6
	cd $(HOME)/src/ghc-$(GHC_VERSION) && make install
