self: super: {
    # If system libraries, e.g. zlib, should be used, we have to enter the
    # nix-shell to build:
    #
    # 	nix-shell -p myHaskell --run 'cabal build'
    #
    # see: https://github.com/haskell/cabal/issues/6228
    myHaskell =
      let
        myGhc = super.haskell.packages.ghc881.ghcWithPackages (haskellPackages: with haskellPackages; [
                  # Some packages require a "pre-build" Haskell library zlib, otherwise
                  # I get:
                  #
                  # 	can't load .so/.DLL for: libz.so (libz.so: cannot open shared object file: No such file or directory)
                  #
                  # even with system zlib declared, see below.
                  zlib
                ]);
      in
        with super; [
          myGhc
          haskellPackages.cabal-install
          haskellPackages.hindent
          # test for hfmt-0.2.3.1 fail
          (haskell.lib.dontCheck haskellPackages.hfmt)
          haskellPackages.hlint
          haskellPackages.stylish-haskell
          # Defining system zlib is an alternative to the Haskell library zlib
          # above and works with most packages. Some Packages howerver only
          # work with "pre-build" Hakell-zlib. The zlib code snippet below
          # could be used to defined nix expression on project bases for static
          # linking.
          zlib
          zlib.dev
          zlib.out
          zlib.static
        ];
}
