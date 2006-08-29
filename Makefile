.PHONY: all configure build install setup clean

all: setup clean configure build

setup: Setup.hs
	ghc -o setup --make ./Setup.hs

clean:
	-./setup clean

configure:
	./setup configure $(CONF) $(USER)

build:
	./setup build

doc:
	./setup haddock

install:
	./setup install $(USER)
