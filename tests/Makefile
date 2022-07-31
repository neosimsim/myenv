test: test-core

TESTS= \
	noesc \
	uniq-paths \
	fix-ex-string \

test-core: $(TESTS)
	which fm
	which o

test-gui: \
	test-core \
	xmonad
	which Afmt

noesc fix-ex-string:
	$@ < $@_test_in | diff $@_test_out -

xmonad:
	xmonad --recompile

uniq-paths:
	! echo $$PATH | sed "s/:/\n/g" | sort | uniq -d | grep .
	! echo $$MANPATH | sed "s/:/\n/g" | sort | uniq -d | grep .

clean:
