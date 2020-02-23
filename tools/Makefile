.POSIX:

PREFIX=$(HOME)
CABAL_FLAGS=--installdir $(PREFIX)/bin --install-method copy --overwrite-policy always
GHC_VERSION=8.6.5
GHC_CONFIG=--disable-ld-override

all:
	$(MAKE) noX
	$(MAKE) xmonad

noX:
	$(MAKE) cabal
	$(MAKE) cabal-env
	$(MAKE) agda
	$(MAKE) pandoc
	$(MAKE) cabal-fmt
	$(MAKE) hfmt

ghcs:
	# You might need to adopt this from system to system, depending on the system installed ghc.
	# Not every ghc version is installable with every ghc version.
	@echo bootsrapping with `which ghc` version `ghc --numeric-version`
	$(MAKE) GHC_VERSION=8.2.2 ghc
	g -v 8.2.2 $(MAKE) GHC_VERSION=8.4.2 ghc
	g -v 8.4.2 $(MAKE) GHC_VERSION=8.6.5 ghc
	g -v 8.6.5 $(MAKE) GHC_VERSION=8.8.2 ghc

hfmt: cabal-update
	# Unable to install hfmt with haskell-src-exts 1.22
	# https://github.com/chrisdone/hindent/issues/562
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) --constraint 'haskell-src-exts < 1.22' hfmt hindent hlint stylish-haskell

cabal-fmt: cabal-update
	cabal v2-install $(CABAL_FLAGS) cabal-fmt

cabal-env: cabal-update
	$(MAKE) -C cabal-extras CABAL_FLAGS="$(INSTALL_FLAGS)" install-cabal-env

agda: cabal-update
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) Agda
	g -v 8.6.5 cabal-env -n agda Agda ieee754 text

xmonad: cabal-update xmobar
	g -v 8.8.2 cabal v2-install $(CABAL_FLAGS) xmonad
	g -v 8.8.2 cabal-env -n xmonad xmonad xmonad-contrib X11

xmobar: cabal-update
	cabal v2-install $(CABAL_FLAGS) -f with_alsa -f with_xft xmobar

pandoc: cabal-update
	cabal v2-install $(CABAL_FLAGS) pandoc pandoc-include-code

alex: cabal-update
	cabal v2-install $(CABAL_FLAGS) alex

happy: cabal-update
	cabal v2-install $(CABAL_FLAGS) happy

ghc: $(PREFIX)/apps/ghc-$(GHC_VERSION)

$(PREFIX)/apps/ghc-$(GHC_VERSION): $(PREFIX)/src/ghc-$(GHC_VERSION)
	$(MAKE) alex happy
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && cp build.mk.sample build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'HADDOCK_DOCS = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'BUILD_SPHINX_HTML = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'BUILD_SPHINX_PDF = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'BUILD_SPHINX_PS = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'INTEGER_LIBRARY = integer-simple' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'BuildFlavour = perf' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && ./configure --prefix=$(PREFIX)/apps/ghc-$(GHC_VERSION) $(GHC_CONFIG)
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && make -j6
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && make install

$(PREFIX)/src/ghc-$(GHC_VERSION):
	mkdir -p $(PREFIX)/src
	cd $(PREFIX)/src && wget https://downloads.haskell.org/~ghc/$(GHC_VERSION)/ghc-$(GHC_VERSION)-src.tar.xz
	cd $(PREFIX)/src && tar xf ghc-$(GHC_VERSION)-src.tar.xz

cabal-update:
	cabal v2-update

cabal: cabal-update
	mkdir -p $(PREFIX)/bin
	cabal v2-install $(CABAL_FLAGS) cabal-install

