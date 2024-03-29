.POSIX:

PREFIX=$(HOME)

MAKE_GHC_VERSION=
MAKE_GHC_VERSIONS=8.2.2 8.4.2 8.6.5 8.8.2 8.8.3 8.8.4 8.10.1
MAKE_AGDA_VERSION=2.6.1
MAKE_AGDA_STDLIB_VERSION=1.3
GHC_CONFIG=--disable-ld-override
BuildFlavour=perf

CABAL_FLAGS=--ghc-options '-j6' --constraint 'hashable -integer-gmp' --constraint 'cryptonite -integer-gmp' --constraint 'scientific +integer-simple' --constraint 'integer-logarithms -integer-gmp'

default: install-core
install: install-core

install-gui:
	$(MAKE) editinacme
	$(MAKE) acmego
	$(MAKE) goimports
	$(MAKE) Watch
	$(MAKE) xplor

install-core:
	$(MAKE) ormolu
	$(MAKE) cabal-fmt
	$(MAKE) hlint
	$(MAKE) apply-refact

install-haskell-env:
	$(MAKE) all-ghc
	$(MAKE) cabal
	$(MAKE) agda
	$(MAKE) agda-stdlib

# Haskell

ormolu \
cabal-fmt \
hlint \
apply-refact:
	cabal v2-install $(CABAL_FLAGS) $@

all-ghc:
	# You might need to adapt MAKE_GHC_VERSIONS.
	@echo bootsrapping with `which ghc` version `ghc --numeric-version`
	# We install the first version in the list twice, but `make` should take care of it.
	versions="$(MAKE_GHC_VERSIONS)"; \
	bootstrapVersion=$${versions%% *}; \
	$(MAKE) MAKE_GHC_VERSION=$$bootstrapVersion ghc; \
	for version in $(MAKE_GHC_VERSIONS); \
	do \
		GHC_VERSION=$$bootstrapVersion $(MAKE) MAKE_GHC_VERSION=$$version ghc; \
		bootstrapVersion=$$version; \
	done

ghc: $(PREFIX)/bin/ghc-$(MAKE_GHC_VERSION)

$(PREFIX)/bin/ghc-$(MAKE_GHC_VERSION): $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/INSTALL.md
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && cp build.mk.sample build.mk
#	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && echo 'HADDOCK_DOCS = YES' >> build.mk
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && echo 'EXTRA_HADDOCK_OPTS += --quickjump --hyperlinked-source' >> build.mk
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && echo 'BUILD_SPHINX_HTML = YES' >> build.mk
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && echo 'BUILD_SPHINX_PDF = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && echo 'BUILD_SPHINX_PS = NO' >> build.mk
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && echo 'INTEGER_LIBRARY = integer-simple' >> build.mk
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/mk && echo 'V = 0' >> build.mk
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION) \
		&& ./configure \
		--prefix=$(PREFIX) \
		--datarootdir=$(PREFIX) \
		--localedir=$(PREFIX) \
		$(GHC_CONFIG)
	# The BuildFlavour file is sourced in build.mk, therefore the BuildFlavour
	# has to be set in the beginning of the file, or passed to make(1). We choose
	# to pass it, to avoid cat(1) hacking.
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION) && make BuildFlavour=$(BuildFlavour) -j6
	cd $(PREFIX)/src/ghc-$(MAKE_GHC_VERSION) && make install

$(PREFIX)/src/ghc-$(MAKE_GHC_VERSION)/INSTALL.md:
	mkdir -p $(PREFIX)/src
	cd $(PREFIX)/src && wget -q https://downloads.haskell.org/~ghc/$(MAKE_GHC_VERSION)/ghc-$(MAKE_GHC_VERSION)-src.tar.xz
	cd $(PREFIX)/src && tar xf ghc-$(MAKE_GHC_VERSION)-src.tar.xz
	cd $(PREFIX)/src && rm ghc-$(MAKE_GHC_VERSION)-src.tar.xz

cabal:
	mkdir -p $(PREFIX)/bin
	cabal v2-update
	# if cabal v2 is currently used, bootstrap v3
	version=$$(cabal --numeric-version); \
	major=$${version%%.*}; \
	if [ "$$major" -lt 3 ]; \
	then \
		cabal v2-install -w ghc-8.6.5 --bindir $(PREFIX)/bin cabal-install; \
		cabal user-config update; \
	fi
#	cabal user-config update -a 'installdir: $(PREFIX)/bin'
#	cabal user-config update -a 'install-method: copy'
#	cabal user-config update -a 'overwrite-policy: always'
#	cabal user-config update -a 'constraint: hashable -integer-gmp, cryptonite -integer-gmp, scientific +integer-simple, integer-logarithms -integer-gmp'
	cabal v2-install -w ghc-8.8.4 $(CABAL_FLAGS) cabal-install

# Haskell Language Server
hls:
	cd $(PREFIX)/bin && wget https://github.com/haskell/haskell-language-server/releases/download/0.5.1/haskell-language-server-Linux-8.10.2.gz
	cd $(PREFIX)/bin && gunzip haskell-language-server-Linux-8.10.2.gz && mv haskell-language-server-Linux-8.10.2 haskell-language-server-8.10.2
	cd $(PREFIX)/bin && chmod +x haskell-language-server-8.10.2

	cd $(PREFIX)/bin && wget https://github.com/haskell/haskell-language-server/releases/download/0.5.1/haskell-language-server-Linux-8.10.1.gz
	cd $(PREFIX)/bin && gunzip haskell-language-server-Linux-8.10.1.gz && mv haskell-language-server-Linux-8.10.1 haskell-language-server-8.10.1
	cd $(PREFIX)/bin && chmod +x haskell-language-server-8.10.1

	cd $(PREFIX)/bin && wget https://github.com/haskell/haskell-language-server/releases/download/0.5.1/haskell-language-server-Linux-8.8.4.gz
	cd $(PREFIX)/bin && gunzip haskell-language-server-Linux-8.8.4.gz && mv haskell-language-server-Linux-8.8.4 haskell-language-server-8.8.4
	cd $(PREFIX)/bin && chmod +x haskell-language-server-8.8.4

	cd $(PREFIX)/bin && wget https://github.com/haskell/haskell-language-server/releases/download/0.5.1/haskell-language-server-Linux-8.8.3.gz
	cd $(PREFIX)/bin && gunzip haskell-language-server-Linux-8.8.3.gz && mv haskell-language-server-Linux-8.8.3 haskell-language-server-8.8.3
	cd $(PREFIX)/bin && chmod +x haskell-language-server-8.8.3

	cd $(PREFIX)/bin && wget https://github.com/haskell/haskell-language-server/releases/download/0.5.1/haskell-language-server-Linux-8.8.2.gz
	cd $(PREFIX)/bin && gunzip haskell-language-server-Linux-8.8.2.gz && mv haskell-language-server-Linux-8.8.2 haskell-language-server-8.8.2
	cd $(PREFIX)/bin && chmod +x haskell-language-server-8.8.2

	cd $(PREFIX)/bin && wget https://github.com/haskell/haskell-language-server/releases/download/0.5.1/haskell-language-server-Linux-8.6.5.gz
	cd $(PREFIX)/bin && gunzip haskell-language-server-Linux-8.6.5.gz && mv haskell-language-server-Linux-8.6.5 haskell-language-server-8.6.5
	cd $(PREFIX)/bin && chmod +x haskell-language-server-8.6.5

	cd $(PREFIX)/bin && wget https://github.com/haskell/haskell-language-server/releases/download/0.5.1/haskell-language-server-wrapper-Linux.gz
	cd $(PREFIX)/bin && gunzip haskell-language-server-wrapper-Linux.gz && mv haskell-language-server-wrapper-Linux haskell-language-server-wrapper
	cd $(PREFIX)/bin && chmod +x haskell-language-server-wrapper

# Agda

agda:
	cabal v2-install -w ghc-8.8.4 $(CABAL_FLAGS) Agda-$(MAKE_AGDA_VERSION)
	rm -f $(HOME)/.ghc/`uname -m`-`uname -s | tr [A-Z] [a-z]`-8.8.4/environments/agda
	cabal v2-install -w ghc-8.8.4 $(CABAL_FLAGS) --lib --package-env agda Agda-$(MAKE_AGDA_VERSION) ieee754

agda-stdlib: $(PREFIX)/lib/agda/agda-stdlib-$(MAKE_AGDA_STDLIB_VERSION)/standard-library.agda-lib
	mkdir -p $(HOME)/.agda
	touch $(HOME)/.agda/libraries-$(MAKE_AGDA_VERSION)
	grep -q standard-library $(HOME)/.agda/libraries-$(MAKE_AGDA_VERSION) || echo $(PREFIX)/lib/agda/agda-stdlib-$(MAKE_AGDA_STDLIB_VERSION)/standard-library.agda-lib >> $(HOME)/.agda/libraries-$(MAKE_AGDA_VERSION)
	touch $(HOME)/.agda/defaults
	grep -q standard-library $(HOME)/.agda/defaults || echo standard-library >> $(HOME)/.agda/defaults

$(PREFIX)/lib/agda/agda-stdlib-$(MAKE_AGDA_STDLIB_VERSION)/standard-library.agda-lib:
	mkdir -p $(PREFIX)/lib/agda
	cd $(PREFIX)/lib/agda && wget -q https://github.com/agda/agda-stdlib/archive/v$(MAKE_AGDA_STDLIB_VERSION).zip
	cd $(PREFIX)/lib/agda && unzip v$(MAKE_AGDA_STDLIB_VERSION).zip
	cd $(PREFIX)/lib/agda && rm v$(MAKE_AGDA_STDLIB_VERSION).zip

# go

acmego:
	go get -u 9fans.net/go/acme/acmego

editinacme:
	go get -u 9fans.net/go/acme/editinacme

goimports:
	go get -u 9fans.net/go/acme/goimports

Watch:
	go get -u 9fans.net/go/acme/Watch

xplor:
	go get -u github.com/mpl/xplor
