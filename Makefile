all:
	cabal install && cabal run
repo:
	new_bitbucket_repo actionkid
spec:
	cabal install && cabal spec
