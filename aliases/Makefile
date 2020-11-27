.POSIX:

PREFIX=$(HOME)

BIN = \
	hwk \
	Watch \

install: phony
	@echo install aliases
	mkdir -p $(PREFIX)/bin/aliases
	cp -f $(BIN) $(PREFIX)/bin/aliases

uninstall: phony
	cd $(PREFIX)/bin/aliases && rm -f $(BIN)

phony:
