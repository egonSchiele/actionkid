# cabal options
OPTIONS=

destroy:
	cabal sandbox delete

init:
	cabal sandbox init

install:
	cabal install $(OPTIONS)

run:
	cabal run

all: install run

spec: install
	cabal spec
