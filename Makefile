idris2 = idris2

build: src stg-idris2.ipkg FORCE
	$(idris2) --build stg-idris2.ipkg

FORCE:

watch:
	while inotifywait -e close_write -r src; do $(idris2) --typecheck stg-idris2.ipkg; done

clean:
	$(idris2) --clean stg-idris2.ipkg
	rm -r build

repl:
	rlwrap $(idris2) --repl stg-idris2.ipkg

test: FORCE
	mkdir -p anf
	mkdir -p stg
	./build/exec/stg-idris2 --cg stg test/Test0.idr -o $(shell pwd)/stg/test0.json --dumpanf anf/test0.anf
