PREFIX=$(HOME)
MANPREFIX=$(PREFIX)/man

# Edit 0/^BIN/+1;/^$/-|sort
BIN = \
	1 \
	2 \
	3 \
	a \
	a+ \
	a- \
	bundle \
	cmnt \
	codespell \
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
	noesc \
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

# MAN = $(BIN:=.1)

install: phony
	mkdir -p $(PREFIX)/bin
	cp -f $(BIN) $(PREFIX)/bin

uninstall: phony
	cd $(PREFIX)/bin && rm -f $(BIN)

phony:
