.POSIX:

DOTFILES_CORE= \
	ghci \
	mkshrc \
	profile \
	tmux.conf \

DOTFILES_GUI= \
	Xmodmap \
	xmonad/build \
	xmonad/xmonad.hs \

# Don't call this or use XDG_CONFIG_HOME on purpose.
# Setting/Using XDG_CONFIG_HOME causes more pain than use.
CONFIG_DIR=$(HOME)/.config

CONFIGFILES_CORE= \
	emacs/init.el \
	fish/config.fish \
	git/attributes \
	git/config \
	git/ignore \

CONFIGFILES_GUI= \
	alacritty/alacritty.yml \
	xmobar/xmobar.hs \

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
	[ -z "$$DISPLAY" ] || 9 9p write plumb/rules <$(HOME)/lib/plumbing || true
	[ -z "$$DISPLAY" ] || xmonad --recompile
	[ -z "$$DISPLAY" ] || xmonad --restart

phony: missing-file

missing-file:
