SRC:=$(shell find . -type f -name '*.hs')
CONFIG:=dist/setup-config
EXECUTABLES:=fake-server server
BINARIES:=$(foreach e,$(EXECUTABLES),dist/build/$(e)/$(e))
ENABLE_TESTS=--enable-tests
OPT=--enable-optimization=2

.PHONY: all fake clean deps

all: $(CONFIG) $(SRC)
	cabal build

fake: all
	dist/build/fake-server/fake-server

clean:
	rm -rf dist

deps:
	cabal install $(ENABLE_TESTS) --only-dep

$(CONFIG): unmatched.cabal
	cabal configure $(ENABLE_TESTS) $(OPT)
