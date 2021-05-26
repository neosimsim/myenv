.POSIX:

DOTFILES_CORE= \
	cabal/config \
	ghci \
	gnupg/gpg-agent.conf \
	mbsyncrc \
	mkshrc \
	mutt/abnrc \
	mutt/mailcap \
	mutt/muttrc \
	mutt/posteorc \
	profile \
	tmux.conf \

DOTFILES_GUI= \
	themes/urxvt/atom-one-light \
	themes/urxvt/tango \
	xinitrc \
	xmobarrc \
	Xmodmap \
	Xmodmap.colemak \
	Xmodmap.qwerty \
	xmonad/xmonad.hs \
	Xresources \
	xsession \

# Don't call this or use XDG_CONFIG_HOME on purpose.
# Setting/Using XDG_CONFIG_HOME causes more pain than use.
CONFIG_DIR=$(HOME)/.config

CONFIGFILES_CORE= \
	emacs/init.el \
	git/config \
	git/ignore \
	vis/visrc.lua \
	vis/themes/peaksea.lua \
	xmonad/build \

CONFIGFILES_GUI= \
	alacritty/alacritty.yml \

DOTFILES=$(DOTFILES_CORE) $(DOTFILES_GUI)
CONFIGFILES=$(CONFIGFILES_CORE) $(CONFIGFILES_GUI)

install: install-core

install-core: phony $(DOTFILES_CORE) $(CONFIGFILES_CORE)

install-gui: $(DOTFILES_GUI) $(CONFIGFILES_GUI) plumbing

$(DOTFILES): phony
	@mkdir -p `dirname $(HOME)/.$@`
	cp $@ $(HOME)/.$@

$(CONFIGFILES): phony
	@mkdir -p `dirname $(CONFIG_DIR)/$@`
	cp $@ $(CONFIG_DIR)/$@

plumbing: phony
	mkdir -p $(HOME)/lib
	cp $@ $(HOME)/lib/$@
	echo >>$(HOME)/lib/$@
	9 sh -c 'echo include $$PLAN9/plumb/basic' >>$(HOME)/lib/$@

uninstall: phony
	for dotfile in $(DOTFILES); do \
		rm -f $(HOME)/.$$dotfile; \
		(rmdir -p $(HOME)/`dirname .$$dotfile` 2>/dev/null || true); \
	done
	for configfile in $(CONFIGFILES); do \
		rm -f $(CONFIG_DIR)/$$configfile; \
		(rmdir -p $(CONFIG_DIR)/`dirname $$configfile` 2>/dev/null || true); \
	done
	rm -f $(HOME)/lib/plumbing

reload-core:
	! tmux list-s >/dev/null 2>&1 || tmux source $(HOME)/.tmux.conf

reload-gui: reload-core
	[ -z "$$DISPLAY" ] || xrdb $(HOME)/.Xresources
	[ -z "$$DISPLAY" ] || ! [ -f $(HOME)/local/Xresources ] || xrdb -merge $(HOME)/local/Xresources
	[ -z "$$DISPLAY" ] || 9 9p write plumb/rules <$(HOME)/lib/plumbing || true
	[ -z "$$DISPLAY" ] || xmonad --recompile
	[ -z "$$DISPLAY" ] || xmonad --restart

phony: missing-file

missing-file:
