.POSIX:

PREFIX=$(HOME)
CABAL_FLAGS=--installdir $(PREFIX)/bin --install-method copy --overwrite-policy always
GHC_VERSION=8.6.5
GHC_CONFIG=

all: noX xmonad

noX: cabal cabal-env agda pandoc cabal-fmt hfmt

ghcs:
	$(MAKE) GHC_VERSION=8.4.2 ghc
	g -v 8.4.2 $(MAKE) GHC_VERSION=8.6.5 ghc
	cd $(PREFIX)/apps && ln -sf ghc-8.6.5 ghc
	g -v 8.6.5 $(MAKE) GHC_VERSION=8.8.2 ghc

hfmt: cabal
	# Unable to install hfmt with haskell-src-exts 1.22
	# https://github.com/chrisdone/hindent/issues/562
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) --constraint 'haskell-src-exts < 1.22' hfmt hindent hlint stylish-haskell

cabal-fmt: cabal
	cabal v2-install $(CABAL_FLAGS) cabal-fmt

cabal-env: cabal
	make -C cabal-extras CABAL_FLAGS="$(INSTALL_FLAGS)" install-cabal-env

agda: cabal
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) Agda
	g -v 8.6.5 cabal-env -n agda ieee text

xmonad: cabal xmobar
	cabal v2-install $(CABAL_FLAGS) xmonad
	cabal-env -n xmonad xmonad xmonad-contrib X11

xmobar: cabal
	cabal v2-install $(CABAL_FLAGS) -f with_alsa -f with_xft xmobar

pandoc: cabal
	cabal v2-install $(CABAL_FLAGS) pandoc pandoc-include-code

alex: cabal
	cabal v2-install $(CABAL_FLAGS) alex

happy: cabal
	cabal v2-install $(CABAL_FLAGS) happy

ghc: $(PREFIX)/src/ghc-$(GHC_VERSION) alex happy
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && ./configure --prefix=$(PREFIX)/apps/ghc-$(GHC_VERSION) $(GHC_CONFIG)
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && make -j6
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && make install

$(PREFIX)/src/ghc-$(GHC_VERSION):
	cd $(PREFIX)/src && wget https://downloads.haskell.org/~ghc/$(GHC_VERSION)/ghc-$(GHC_VERSION)-src.tar.xz
	cd $(PREFIX)/src && tar xf ghc-$(GHC_VERSION)-src.tar.xz

cabal:
	mkdir -p $(PREFIX)/bin
	cabal v2-update
	cabal v2-install $(CABAL_FLAGS) cabal-install

