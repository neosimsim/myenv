PREFIX=$(HOME)
MANPREFIX=$(PREFIX)/man

POSIX_BIN = \
	1 \
	2 \
	3 \
	a \
	a+ \
	a- \
	agda \
	agda-mode \
	agda2 \
	agofmt \
	Ahfmt \
	bat \
	bundle \
	cmnt \
	codespell \
	dpass \
	fext \
	filter \
	g \
	games \
	getmod \
	git-to-dot \
	hi \
	hsinit \
	keystore \
	lessk \
	license \
	lower \
	mkgit \
	open \
	overwrite \
	passgen \
	pick \
	quote \
	remtrail \
	rotatekb \
	splitcammel \
	tmux-run-selection-with \
	tmux-select-word \
	tmux-ws \
	ucmnt \
	unfreeze-ghc-libs \
	upper \
	xname \
	year \
	zap \

HASKELL_BIN = \
	uni \
	git-files \

BIN = \
	$(POSIX_BIN) \
	$(HASKELL_BIN) \

# MAN = $(BIN:=.1)

install: install-posix install-haskell

install-posix: phony
	@echo install posix compatible scripts
	mkdir -p $(PREFIX)/bin
	cp -f $(POSIX_BIN) $(PREFIX)/bin

install-haskell: phony
	@echo install Haskell scripts
	cabal v2-install --installdir $(PREFIX)/bin --install-method copy

uninstall: phony
	cd $(PREFIX)/bin && rm -f $(BIN)

TESTS=\
	  test_uni

test: $(TESTS)

test_uni: phony
	cabal run -v0 exe:uni < uni_test_in | diff - uni_test_out
	@echo test broken pipes
	cabal run -v0 exe:uni < uni_test_in | sed 2q >/dev/null

verify:
	cabal-fmt scripts.cabal | diff scripts.cabal -
	find . -name '*.hs' | xargs hfmt

verify-apply:
	cabal-fmt -i scripts.cabal
	find . -name '*.hs' | xargs hfmt -w

phony:
