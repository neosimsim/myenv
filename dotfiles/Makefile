.POSIX:

DOTFILES_CORE= \
	cabal/config \
	config/git/config \
	config/git/ignore \
	config/nixpkgs/config.nix \
	config/vis/visrc.lua \
	config/vis/themes/peaksea.lua \
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
	config/alacritty/alacritty.yml \
	plumbing \
	themes/urxvt/atom-one-light \
	themes/urxvt/tango \
	xinitrc \
	xmobarrc \
	Xmodmap \
	Xmodmap.colemak \
	Xmodmap.qwerty \
	xmonad/build \
	xmonad/xmonad.hs \
	Xresources \
	xsession \

DOTFILES=$(DOTFILES_CORE) $(DOTFILES_GUI)

default: install-core
install: install-core

install-core: phony $(DOTFILES_CORE)
	! tmux list-s >/dev/null 2>&1 || tmux source $(HOME)/.tmux.conf

install-gui: install-core $(DOTFILES_GUI) plumbing
	[ -z "$$DISPLAY" ] || 9p write plumb/rules <$(HOME)/.plumbing
	[ -z "$$DISPLAY" ] || xrdb $(HOME)/.Xresources
	[ -z "$$DISPLAY" ] || ! [ -f $(HOME)/.Xresources.local ] || xrdb -merge $(HOME)/.Xresources.local
	[ -z "$$DISPLAY" ] || xmonad --recompile
	[ -z "$$DISPLAY" ] || xmonad --restart

$(DOTFILES): phony
	@mkdir -p `dirname $(HOME)/.$@`
	cp $@ $(HOME)/.$@

uninstall: phony
	for dotfile in $(DOTFILES); do \
		rm -f $(HOME)/.$$dotfile; \
		(rmdir -p $(HOME)/`dirname .$$dotfile` 2>/dev/null || true); \
	done

phony: missing-file

missing-file:
