.PHONY: all test doc install clean

all:
	dune build --display=short @all

test:
	dune runtest -j1 --no-buffer

doc:
	dune build --dev @doc

install:
	dune install

clean:
	dune clean
