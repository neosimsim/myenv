# NeoSimSim Env
TL;DR

	make install && make test

Scripts to install tools and configurations I use on a daily basis.

To install everything, you need

- nix
- ghc <= 8.6.5
- cabal >=2.4
- go

e. g. installed from you systems packages manager.

If your distribution is supported, you can run the preparation script, e. g.

	sh prepare.alpine

# Targets

- `install-gui` installs core tools and gui tools
- `install-core` installs packages, which don't require X11 libraries, e. g. xmonad.
- `install` falls back to `install-core`

# GHC
## `GHC_VERSIONS`
Depending on the installed GHC version you have to set `GHC_VERSIONS`.
It is recommended to start with the system installed version.
The list has to be in ascending order and has to contain 8.6.5
and 8.8.2. Assuming your system has GHC 8.2.2 install you should
call `make` with

	make GHC_VERSIONS='8.2.2 8.4.2 8.6.5 8.8.2' install

# Customizations
You can specify workstation specific settings in $HOME/.profile.local, e. g.

	export BROWSER=firefox
	export ACME_VARFONT=/mnt/font/FreeSans/12a/font
	export ACME_FIXFONT=/mnt/font/DejaVuSansMono/13a/font

# Troubleshooting
## unknown package `interger-gmp`
ghc is compiled with `INTEGER_LIBRARY = integer-simple`. This might cause
trouble if cabal already cached some packages built with integer-gmp. In this
case it might help to remove *$HOME/.cabal/store* and *$HOME/.cabal/packages*.

# Dockerfiles
The Dockerfiles are not actually meant to build production images. There shall
only verify that my setup works in my supported environment.
