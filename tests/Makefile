test-core: agda
test-gui: test-core xmonad

agda:
	agda --library standard-library -i . -c hello-agda.agda
	./hello-agda

xmonad:
	xmonad --recompile

uniq-paths:
	$(SHELL) -c '! echo $$PATH | sed "s/:/\n/g" | sort | uniq -d | grep .'
	$(SHELL) -c '! echo $$MANPATH | sed "s/:/\n/g" | sort | uniq -d | grep .'

clean:
	rm -f hello-agda *.agdai
	rm -rf MAlonzo

