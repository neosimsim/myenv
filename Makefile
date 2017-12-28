PREFIX=$(HOME)/.local
MANPREFIX=$(PREFIX)/share/man

BIN = mkgit \
	remtrail \
	bat \
	hi \
	xname \
	keystore \
	passgen \
	dpass

MAN = $(BIN:=.1)
MAN = mkgit

install:
	mkdir -p $(PREFIX)/bin
	cp $(BIN) $(PREFIX)/bin
	mkdir -p $(MANPREFIX)/man1
	cp $(MAN) $(MANPREFIX)/man1

uninstall:
	cd $(PREFIX)/bin && rm -f $(BIN)
	cd $(MANPREFIX)/man1 && rm -f $(MAN)

.PHONY: install uninstall
