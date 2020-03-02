# NeoSimSim Env
Scripts to install tools and configurations I use on a daily basis.

To install everything, you need

- ghc <= 8.6.5
- cabal >=2.4
- go

e. g. installed from you systems packages manager.

If your distribution is supported, you can run the preparation script, e. g.

	sh prepare.alpine

# Targets

- `install` installs everything
- `install-noX` installs packages, which don't require X11 libraries, e. g. xmonad.

# GHC
## `GHC_VERSIONS`
Depending on the installed GHC version you have to set `GHC_VERSIONS`.
It is recommended to start with the system installed version.
The list has to be in ascending order and has to contain 8.6.5
and 8.8.2. Assuming your system has GHC 8.2.2 install you should
call `make` with

	make GHC_VERSIONS='8.2.2 8.4.2 8.6.5 8.8.2' install

# Troubleshooting
## unknown package `interger-gmp`
ghc is compiled with `INTEGER_LIBRARY = integer-simple`. This might cause
trouble if cabal already cached some packages built with integer-gmp. In this
case it might help to remove *$HOME/.cabal/store* and *$HOME/.cabal/packages*.
