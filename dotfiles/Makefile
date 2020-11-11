.POSIX:

DOTFILES= \
	config/konsolerc \
	config/nixpkgs/overlays/emacs.nix \
	config/nixpkgs/overlays/haskell.nix \
	config/nixpkgs/overlays/xmobar.nix \
	config/nixpkgs/overlays/xmonad.nix \
	ctags \
	cabal/config \
	emacs \
	ghci \
	gitconfig \
	gitignore_global \
	i3.conf \
	i3status.conf \
	local/share/konsole/Mono.profile \
	local/share/konsole/SolarizedLightContrast.colorscheme \
	local/share/konsole/Var.profile \
	mbsyncrc \
	mlterm/aafont \
	mlterm/color \
	mlterm/key \
	mlterm/main \
	mlterm/vaafont \
	mutt/abnrc \
	mutt/mailcap \
	mutt/muttrc \
	mutt/posteorc \
	npmrc \
	plumbing \
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
	xinitrc \
	xmobarrc \
	Xmodmap \
	xmonad/build \
	xmonad/xmonad.hs \
	Xresources \

install: phony $(DOTFILES)

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
