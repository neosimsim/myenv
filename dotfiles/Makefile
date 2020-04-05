.POSIX:

# config files for tools supporting XGD_CONFIG_HOME
CONFIGFILES=\
	xmonad/build \
	xmonad/xmonad.hs \
	plumbing \
	nixpkgs/config.nix \
	nixpkgs/overlays/emacs.nix \
	nixpkgs/overlays/haskell.nix \
	nixpkgs/overlays/xmobar.nix \
	nixpkgs/overlays/xmonad.nix \
	git/config \
	git/ignore \
	mutt/abnrc \
	mutt/mailcap \
	mutt/muttrc \
	mutt/posteorc \
	mbsyncrc \
	Xmodmap \
	Xresources \
	vim/autoload/plug.vim \
	vim/autoload/statusline.vim \
	vim/colors/neosimsim.vim \
	vim/ftdetect/gnuplot.vim \
	vim/ftdetect/markdown.vim \
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
	vim/vimrc \
	shinit \
	xmobarrc \

# legacy tools which have to be stored in $HOME as "hidden" file
DOTFILES= \
	ctags \
	ghci \
	mlterm/aafont \
	mlterm/color \
	mlterm/key \
	mlterm/main \
	mlterm/vaafont \
	profile \
	tmux.conf \
	xinitrc \

#	mutt/support \
#	mutt/sig \
#	mutt/default \
#	pandoc/templates/neosimsim.latex \

install: phony $(DOTFILES) $(CONFIGFILES)

$(CONFIGFILES): phony
	@mkdir -p `dirname $(HOME)/lib/$@`
	cp $@ $(HOME)/lib/$@
	@mkdir -p $(HOME)/var/vim/swap
	@mkdir -p $(HOME)/var/vim/undo
	@mkdir -p $(HOME)/var/vim/backup

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
	for dotfile in $(DOTFILES); do \
		rm -f $(HOME)/lib/$$dotfile; \
		(rmdir -p $(HOME)/`dirname lib/$$dotfile` 2>/dev/null || true); \
	done

phony: missing-file

missing-file:
