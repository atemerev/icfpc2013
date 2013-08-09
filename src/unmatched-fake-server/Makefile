SRC:=$(shell find Unmatched -type f -name '*.hs')
CONFIG:=dist/setup-config
BINARY:=dist/build/fake-server/fake-server

.PHONY: all run clean

all: $(BINARY)

run: all
	$(BINARY)

clean:
	rm -rf dist

$(CONFIG): unmatched-fake-server.cabal
	cabal configure

$(BINARY): $(CONFIG) $(SRC)
	cabal build

