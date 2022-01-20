PREFIX=$(HOME)
MANPREFIX=$(PREFIX)/man

# Edit 0/^POSIX_BIN/+1;/^$/-|sort
POSIX_BIN = \
	1 \
	2 \
	3 \
	a \
	a+ \
	a- \
	bundle \
	cmnt \
	codespell \
	dpass \
	ec \
	fext \
	filter \
	find-ex-function \
	find-ex-module \
	find-match \
	fm \
	Font+ \
	Font- \
	fzf-open \
	fzf-tags \
	games \
	gen-vim-uni \
	gen-vis-uni \
	getmod \
	git-no-unstaged \
	git-to-dot \
	hsinit \
	indirs \
	license \
	lower \
	memusage \
	NewW \
	nocolor \
	now \
	open \
	overwrite \
	passgen \
	quote \
	remtrail \
	rotatekb \
	snipe \
	splitcammel \
	t \
	tmux-run-selection-with \
	tmux-sos \
	tmux-split-pipe \
	tmux-ws \
	today \
	ucmnt \
	unfreeze-ghc-libs \
	upper \
	year \

BIN = \
	$(POSIX_BIN) \

# MAN = $(BIN:=.1)

install: install-posix install-haskell

install-posix: phony
	@echo install posix compatible scripts
	mkdir -p $(PREFIX)/bin
	cp -f $(POSIX_BIN) $(PREFIX)/bin

install-haskell: phony test-haskell
	@echo install Haskell scripts
	cabal v2-install --installdir $(PREFIX)/bin --install-method copy --overwrite-policy always

uninstall: phony
	cd $(PREFIX)/bin && rm -f $(BIN)

TESTS=\

test: $(TESTS) test-haskell

test-haskell:
	cabal v2-test
	cabal v2-run -v0 exe:uni < uni_test_in | diff - uni_test_out
	@echo test broken pipes
	cabal v2-run -v0 exe:uni < uni_test_in | sed 2q >/dev/null
	@echo test print
	cabal v2-run -v0 exe:uni -- print | diff - uni_print_test_out
	@echo test print broken pipes
	cabal v2-run -v0 exe:uni -- print | sed 2q >/dev/null

verify:
	cabal-fmt scripts.cabal | diff scripts.cabal -
	find . -name '*.hs' | xargs hfmt

verify-apply:
	cabal-fmt -i scripts.cabal
	find . -name '*.hs' | xargs hfmt -w

phony:
