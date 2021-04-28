# NeoSimSim Env
TL;DR

	make install && make test

Scripts to install tools and configurations I use on a daily basis.

To install everything, you need

- nix
- go

e. g. installed from you systems packages manager.

If you want to setup the Haskell environment you need

- ghc <= 8.6.5 (depending on the MAKE_GHC_VERSION in tools/Makefile)
- cabal >=2.4

You can then run

	make install-haskell-env

If your distribution is supported, you can run the preparation script, e. g.

	sh prepare.alpine

# Targets

- `install` falls back to `install-core`
- `install-core` installs packages, which don't require X11 libraries.
- `install-gui` installs core tools and gui tools
- `install-haskell-env` installs GHC, cabal and useful development tools.

# GHC
## `GHC_VERSIONS`
Depending on the installed GHC version you have to set `GHC_VERSIONS`.
It is recommended to start with the system installed version.
The list has to be in ascending order and has to contain 8.6.5
and 8.8.2. Assuming your system has GHC 8.2.2 install you should
call `make` with

	make GHC_VERSIONS='8.2.2 8.4.2 8.6.5 8.8.2' install-haskell-env

# Customizations
You can specify workstation specific settings in $HOME/local/profile, e. g.

	export BROWSER=firefox
	export ACME_VARFONT=/mnt/font/FreeSans/12a/font
	export ACME_FIXFONT=/mnt/font/DejaVuSansMono/13a/font

# Troubleshooting
## unknown package `interger-gmp`
ghc is compiled with `INTEGER_LIBRARY = integer-simple`. This might cause
trouble if cabal already cached some packages built with integer-gmp. In this
case it might help to remove *$HOME/.cabal/store* and *$HOME/.cabal/packages*.

## nix: `warning: setlocale:
If you get the following error, when envoking nix command, e.g. `nix-shell`

	bash: warning: setlocale: LC_ALL: cannot change locale (en_US.UTF-8)

add the following to `$HOME/local/profile`

	export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

You may have to adopt `/usr/lib/locale`.

When on a non glibc Linux, e.g. alpine try to install

	nix-env -i glibc-locales

and add the following to `$HOME/local/profile`

	export LOCALE_ARCHIVE="$(nix-env --installed --no-name --out-path --query glibc-locales)/lib/locale/locale-archive"

Compare with [NixOS/nix#599](https://github.com/NixOS/nix/issues/599).

# Dockerfiles
The Dockerfiles are not actually meant to build production images. There shall
only verify that my setup works in my supported environment.

