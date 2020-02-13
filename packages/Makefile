.POSIX:

PREFIX=$(HOME)
CABAL_FLAGS=--installdir $(PREFIX)/bin --install-method copy

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
	g -v 8.6.5 cabal-env -n agda ieee

