# NeoSimSim Env
Scripts to install tools and configurations I use on a daily basis.

To install everything you need

- ghc-8.0.2, ghc-8.4.2 or ghc-8.6.5
- cabal >2.4
- go

i. e. installed from you systems packages manager.

Depending on the installed GHC version already installed and to install
you may want to adapt the `GHC_VERSIONS` in [tools/Makefile](tools/Makefile):

	make GHC_VERSIONS='8.6.5 8.8.2' install

or

	make GHC_VERSIONS='8.6.5 8.8.2' install-noX

if you don't have or need X11 tools, e. g. xmonad.

Note that 8.6.5 8.8.2 are required.

# Troubleshooting
## unknown package `interger-gmp`
ghc is compiled with `INTEGER_LIBRARY = integer-simple`. This might cause
trouble if cabal already cached some packages built with integer-gmp. In this
case it might help to remove *$HOME/.cabal/store* and *$HOME/.cabal/packages*.
