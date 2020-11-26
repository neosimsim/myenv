all: agda xmonad

agda:
	nix-shell --pure -p 'agda.withPackages (p: [ p.standard-library ])' --run 'agda --library standard-library -i . hello-agda.agda'
	./hello-agda

xmonad:
	xmonad --recompile

clean:
	rm -f hello-agda *.agdai
	rm -rf MAlonzo

