.POSIX:

PREFIX=$(HOME)

GHC_VERSION=8.6.5
GHC_VERSIONS=8.2.2 8.4.2 8.6.5 8.8.2 8.8.3
GHC_CONFIG=--disable-ld-override
BuildFlavour=perf

CABAL_FLAGS=--installdir $(PREFIX)/bin --install-method copy --overwrite-policy always --constraint 'hashable -integer-gmp' --constraint 'cryptonite -integer-gmp' --constraint 'scientific +integer-simple' --constraint 'integer-logarithms -integer-gmp'

all:
	$(MAKE) noX
	$(MAKE) X

X:
	$(MAKE) xmonad
	$(MAKE) plan9port
	$(MAKE) editinacme
	$(MAKE) Watch

noX:
	$(MAKE) all-ghc
	$(MAKE) cabal
	$(MAKE) agda
	$(MAKE) agda-stdlib
	$(MAKE) pandoc
	$(MAKE) cabal-fmt
	$(MAKE) hfmt

# Haskell

hfmt:
	# Unable to install hfmt with haskell-src-exts 1.22
	# https://github.com/chrisdone/hindent/issues/562
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) --constraint 'haskell-src-exts < 1.22' hfmt hindent hlint stylish-haskell

cabal-fmt:
	g -v 8.8.2 cabal v2-install $(CABAL_FLAGS) cabal-fmt

xmonad: xmobar
	g -v 8.8.2 cabal v2-install $(CABAL_FLAGS) xmonad
	g -v 8.8.2 cabal v2-install $(CABAL_FLAGS) --lib --package-env xmonad xmonad xmonad-contrib X11

xmobar:
	g -v 8.8.2 cabal v2-install $(CABAL_FLAGS) -f with_alsa -f with_xft xmobar

pandoc:
	g -v 8.8.2 cabal v2-install $(CABAL_FLAGS) pandoc pandoc-include-code

all-ghc:
	# You might need to adopt GHC_VERSIONS.
	@echo bootsrapping with `which ghc` version `ghc --numeric-version`
	# We install the first version in the list twice, but `make` should take care of it.
	versions="$(GHC_VERSIONS)"; \
	bootstrapVersion=$${versions%% *}; \
	$(MAKE) GHC_VERSION=$$bootstrapVersion ghc; \
	for version in $(GHC_VERSIONS); \
	do \
		g -v $$bootstrapVersion $(MAKE) GHC_VERSION=$$version ghc; \
		bootstrapVersion=$$version; \
	done

ghc: $(PREFIX)/apps/ghc-$(GHC_VERSION)/bin/ghc

$(PREFIX)/apps/ghc-$(GHC_VERSION)/bin/ghc: $(PREFIX)/src/ghc-$(GHC_VERSION)/INSTALL.md
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && cp build.mk.sample build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'HADDOCK_DOCS = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'BUILD_SPHINX_HTML = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'BUILD_SPHINX_PDF = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'BUILD_SPHINX_PS = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'INTEGER_LIBRARY = integer-simple' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION)/mk && echo 'V = 0' >> build.mk
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && ./configure --prefix=$(PREFIX)/apps/ghc-$(GHC_VERSION) $(GHC_CONFIG)
	# The BuildFlavour file is sourced in build.mk, therefore the BuildFlavour
	# has to be set in the beginning of the file, or passed to make(1). We choose
	# to pass it, to avoid cat(1) hacking.
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && make BuildFlavour=$(BuildFlavour) -j6
	cd $(PREFIX)/src/ghc-$(GHC_VERSION) && make install

$(PREFIX)/src/ghc-$(GHC_VERSION)/INSTALL.md:
	mkdir -p $(PREFIX)/src
	cd $(PREFIX)/src && wget -q https://downloads.haskell.org/~ghc/$(GHC_VERSION)/ghc-$(GHC_VERSION)-src.tar.xz
	cd $(PREFIX)/src && tar xf ghc-$(GHC_VERSION)-src.tar.xz
	cd $(PREFIX)/src && rm ghc-$(GHC_VERSION)-src.tar.xz

cabal:
	mkdir -p $(PREFIX)/bin
	cabal v2-update
	# if cabal v2 is currently used, bootstrap v3
	version=$$(cabal --numeric-version); \
	major=$${version%%.*}; \
	if [ "$$major" -lt 3 ]; \
	then \
		g -v 8.6.5 cabal v2-install --bindir $(PREFIX)/bin cabal-install; \
		cabal user-config update; \
	fi
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) cabal-install

# Agda

agda:
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) Agda
	g -v 8.6.5 cabal v2-install $(CABAL_FLAGS) --lib --package-env agda Agda ieee754 text

agda-stdlib: $(PREFIX)/src/agda/std-lib/standard-library.agda-lib
	mkdir -p $(HOME)/.agda
	grep -q standard-library $(HOME)/.agda/libraries || echo $(PREFIX)/src/agda/std-lib/standard-library.agda-lib >> $(HOME)/.agda/libraries
	grep -q standard-library $(HOME)/.agda/defaults || echo standard-library >> $(HOME)/.agda/defaults

 $(PREFIX)/src/agda/std-lib/standard-library.agda-lib:
	cd  $(PREFIX)/src/ && git clone --recursive https://github.com/agda/agda

# go

editinacme:
	go get 9fans.net/go/acme/editinacme

Watch:
	go get github.com/eaburns/Watch


# Plan9

plan9port: $(PREFIX)/apps/plan9port/bin/9p

$(PREFIX)/apps/plan9port/bin/9p: $(PREFIX)/apps/plan9port/INSTALL
	cd $(PREFIX)/apps/plan9port && ./INSTALL

$(PREFIX)/apps/plan9port/INSTALL:
	cd  $(PREFIX)/apps/ && git clone --recursive https://github.com/9fans/plan9port
