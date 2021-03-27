
build: src stg-idris2.ipkg FORCE
	idris2 --build stg-idris2.ipkg

FORCE:

clean:
	idris2 --clean stg-idris2.ipkg
	rm -r build

repl:
	rlwrap idris2 --repl stg-idris2.ipkg

test: FORCE
	mkdir -p anf
	./build/exec/stg-idris2 --cg stg test/Test0.idr -o $(shell pwd)/stg/test0.json --dumpanf anf/test0.anf
