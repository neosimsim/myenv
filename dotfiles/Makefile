.POSIX:

XDG_CONFIG_HOME=$(HOME)/lib

DOTFILES_CORE= \
	cabal/config \
	ghci \
	gnupg/gpg-agent.conf \
	mbsyncrc \
	mutt/abnrc \
	mutt/mailcap \
	mutt/muttrc \
	mutt/posteorc \
	profile \
	shinit \
	tmux.conf \

DOTFILES_GUI= \
	themes/urxvt/atom-one-light \
	themes/urxvt/tango \
	xinitrc \
	xmobarrc \
	Xmodmap \
	Xmodmap.colemak \
	Xmodmap.qwerty \
	Xresources \
	xsession \

XDGFILES_CORE= \
	git/config \
	git/ignore \
	vis/visrc.lua \
	vis/themes/peaksea.lua \
	xmonad/build \
	xmonad/xmonad.hs \

XDGFILES_GUI= \
	alacritty/alacritty.yml \

DOTFILES=$(DOTFILES_CORE) $(DOTFILES_GUI)
XDGFILES=$(XDGFILES_CORE) $(XDGFILES_GUI)

default: install-core
install: install-core

install-core: phony $(DOTFILES_CORE) $(XDGFILES_CORE)
	! tmux list-s >/dev/null 2>&1 || tmux source $(HOME)/.tmux.conf

install-gui: install-core $(DOTFILES_GUI) $(XDGFILES_GUI) plumbing
	[ -z "$$DISPLAY" ] || 9 9p write plumb/rules <$(HOME)/lib/plumbing
	[ -z "$$DISPLAY" ] || xrdb $(HOME)/.Xresources
	[ -z "$$DISPLAY" ] || ! [ -f $(HOME)/local/Xresources ] || xrdb -merge $(HOME)/local/Xresources
	# alternatively one could set XMONAD_CACHE_DIR etc.
	mkdir -p $(XDG_CACHE_HOME)/xmonad
	[ -z "$$DISPLAY" ] || xmonad --recompile
	[ -z "$$DISPLAY" ] || xmonad --restart

$(DOTFILES): phony
	@mkdir -p `dirname $(HOME)/.$@`
	cp $@ $(HOME)/.$@

$(XDGFILES): phony
	@mkdir -p `dirname $(XDG_CONFIG_HOME)/$@`
	cp $@ $(XDG_CONFIG_HOME)/$@

plumbing: phony
	mkdir -p $(HOME)/lib
	cp $@ $(HOME)/lib/$@

uninstall: phony
	for dotfile in $(DOTFILES); do \
		rm -f $(HOME)/.$$dotfile; \
		(rmdir -p $(HOME)/`dirname .$$dotfile` 2>/dev/null || true); \
	done
	for xdgfile in $(XDGFILES); do \
		rm -f $(XDG_CONFIG_HOME)/$$xdgfile; \
		(rmdir -p $(XDG_CONFIG_HOME)/`dirname $$xdgfile` 2>/dev/null || true); \
	done
	rm -f $(HOME)/lib/plumbing

phony: missing-file

missing-file:
