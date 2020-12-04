.POSIX:

DOTFILES_CORE= \
	config/nixpkgs/config.nix \
	ctags \
	cabal/config \
	emacs \
	ghci \
	gitconfig \
	gitignore_global \
	mbsyncrc \
	mutt/abnrc \
	mutt/mailcap \
	mutt/muttrc \
	mutt/posteorc \
	npmrc \
	profile \
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
	vim/spell/de.utf-8.add \
	vim/spell/en.utf-8.add \
	vim/syntax/tex.vim \
	vimrc \

DOTFILES_GUI= \
	config/konsolerc \
	i3.conf \
	i3status.conf \
	local/share/konsole/Mono.profile \
	local/share/konsole/SolarizedLightContrast.colorscheme \
	local/share/konsole/Var.profile \
	mlterm/aafont \
	mlterm/color \
	mlterm/key \
	mlterm/main \
	mlterm/vaafont \
	plumbing \
	xinitrc \
	xmobarrc \
	Xmodmap \
	xmonad/build \
	xmonad/shell.nix \
	xmonad/xmonad.hs \
	Xresources \

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
