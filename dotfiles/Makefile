.POSIX:

DOTFILES_CORE= \
	cabal/config \
	config/nixpkgs/config.nix \
        config/vis/visrc.lua \
	ghci \
	gitconfig \
	gitignore_global \
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
	plumbing \
	xinitrc \
	xmobarrc \
	Xmodmap \
	xmonad/build \
	xmonad/shell.nix \
	xmonad/xmonad.hs \
	Xresources \
	xsession \

DOTFILES=$(DOTFILES_CORE) $(DOTFILES_GUI)

default: install-core
install: install-core
install-core: phony $(DOTFILES_CORE)
install-gui: install-core $(DOTFILES_GUI)

$(DOTFILES): phony
	@mkdir -p `dirname $(HOME)/.$@`
	cp $@ $(HOME)/.$@
	@mkdir -p $(HOME)/.vim/swap
	@mkdir -p $(HOME)/.vim/undo
	@mkdir -p $(HOME)/.vim/backup

uninstall: phony
	for dotfile in $(DOTFILES); do \
		rm -f $(HOME)/.$$dotfile; \
		(rmdir -p $(HOME)/`dirname .$$dotfile` 2>/dev/null || true); \
	done

phony: missing-file

missing-file:
