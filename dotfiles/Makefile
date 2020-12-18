.POSIX:

DOTFILES_CORE= \
	cabal/config \
	config/nixpkgs/config.nix \
        config/vis/visrc.lua \
	ctags \
	emacs \
	ghci \
	gitconfig \
	gitignore_global \
	gnupg/gpg-agent.conf \
	mbsyncrc \
	mutt/abnrc \
	mutt/mailcap \
	mutt/muttrc \
	mutt/posteorc \
	npmrc \
	profile \
	shinit \
	tmux.conf \
	vim/autoload/plug.vim \
	vim/autoload/statusline.vim \
	vim/colors/neosimsim.vim \
	vim/ftdetect/gnuplot.vim \
	vim/ftdetect/markdown.vim \
	vim/ftdetect/yaml.vim \
	vim/ftplugin/agda.vim \
	vim/ftplugin/git.vim \
	vim/ftplugin/go.vim \
	vim/ftplugin/haskell.vim \
	vim/ftplugin/help.vim \
	vim/ftplugin/mail.vim \
	vim/ftplugin/markdown.vim \
	vim/ftplugin/nerdtree.vim \
	vim/ftplugin/tex.vim \
	vim/ftplugin/yaml.vim \
	vim/plugin/pipe.vim \
	vim/plugin/rename.vim \
	vimrc \
	vim/spell/de.utf-8.add \
	vim/spell/en.utf-8.add \
	vim/syntax/tex.vim \

DOTFILES_GUI= \
	i3.conf \
	i3status.conf \
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
