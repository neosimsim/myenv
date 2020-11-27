test-core: agda uni-vim-exists
test-gui: xmonad

agda:
	nix-shell --pure -p 'agda.withPackages (p: [ p.standard-library ])' --run 'agda --library standard-library -i . hello-agda.agda'
	./hello-agda

xmonad:
	xmonad --recompile

uni-vim-exists:
	test -f $(HOME)/.vim/uni.vim

uniq-paths:
	$(SHELL) -c '! echo $$PATH | sed "s/:/\n/g" | sort | uniq -d | grep .'
	$(SHELL) -c '! echo $$MANPATH | sed "s/:/\n/g" | sort | uniq -d | grep .'

clean:
	rm -f hello-agda *.agdai
	rm -rf MAlonzo

