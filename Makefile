
build: src stg-idris2.ipkg
	idris2 --build stg-idris2.ipkg

clean:
	idris2 --clean stg-idris2.ipkg

repl:
	idris2 --repl stg-idris2.ipkg

test:
	./build/exec/stg-idris2 --cg stg src/Hello.idr -o hello.json
