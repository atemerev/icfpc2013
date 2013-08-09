SRC:=$(shell find . -type f -name '*.hs')
CONFIG:=dist/setup-config
EXECUTABLES:=fake-server
BINARIES:=$(foreach e,$(EXECUTABLES),dist/build/$(e)/$(e))

.PHONY: all fake clean

all: $(CONFIG) $(SRC)
	cabal build

fake: all
	dist/build/fake-server/fake-server

clean:
	rm -rf dist

$(CONFIG): unmatched.cabal
	cabal configure

