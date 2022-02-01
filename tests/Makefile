test: test-core

test-core: \
	agda \
	noesc \
	uniq-paths
	which fm
	which o

test-gui: \
	test-core \
	xmonad
	which Afmt

noesc:
	noesc < noesc_test_in | diff noesc_test_out -

agda:
	agda --library standard-library -i . -c hello-agda.agda
	./hello-agda

xmonad:
	xmonad --recompile

uniq-paths:
	! echo $$PATH | sed "s/:/\n/g" | sort | uniq -d | grep .
	! echo $$MANPATH | sed "s/:/\n/g" | sort | uniq -d | grep .

clean:
	rm -f hello-agda *.agdai
	rm -rf MAlonzo

