#TODO integrate into Setup.hs

verify:
	cabal-fmt scripts.cabal | diff scripts.cabal -
	find . -name '*.hs' -not -path './dist/*' | xargs ormolu -m check

verify-apply:
	cabal-fmt -i scripts.cabal
	find . -name '*.hs' -not -path './dist/*' | xargs ormolu -i
