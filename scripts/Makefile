PREFIX=$(HOME)
MANPREFIX=$(PREFIX)/man

# Edit .+2;/^$/-|sort
POSIX_BIN = \
	1 \
	2 \
	3 \
	a \
	a+ \
	a- \
	agofmt \
	bat \
	bundle \
	cmnt \
	codespell \
	dpass \
	fext \
	filter \
	games \
	gen-vim-uni \
	getmod \
	hsfmt \
	git-to-dot \
	hi \
	Hsfmt \
	hsinit \
	keystore \
	lessk \
	license \
	lower \
	mkgit \
	open \
	overwrite \
	passgen \
	pg \
	pick \
	quote \
	remtrail \
	rotatekb \
	splitcammel \
	startpg \
	tmux-run-selection-with \
	tmux-select-word \
	tmux-ws \
	ucmnt \
	unfreeze-ghc-libs \
	upper \
	xname \
	year \
	zap \

# Edit .+2;/^$/-|sort
HASKELL_BIN = \
	git-files \
	uni \

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
	nix-shell --pure --command "cabal v2-install --installdir $(PREFIX)/bin --install-method copy --overwrite-policy always"

uninstall: phony
	cd $(PREFIX)/bin && rm -f $(BIN)

TESTS=\
	  test_uni

test: $(TESTS)

test_uni: phony
	nix-shell --command 'cabal v2-run -v0 exe:uni' < uni_test_in | diff - uni_test_out
	@echo test broken pipes
	nix-shell --command 'cabal v2-run -v0 exe:uni' < uni_test_in | sed 2q >/dev/null
	@echo test print
	nix-shell --command 'cabal v2-run -v0 exe:uni -- print' | diff - uni_print_test_out
	@echo test print broken pipes
	nix-shell --command 'cabal v2-run -v0 exe:uni -- print' | sed 2q >/dev/null

verify:
	cabal-fmt scripts.cabal | diff scripts.cabal -
	find . -name '*.hs' | xargs hfmt

verify-apply:
	cabal-fmt -i scripts.cabal
	find . -name '*.hs' | xargs hfmt -w

phony:
