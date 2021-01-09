
build: src stg-idris2.ipkg
	idris2 --build stg-idris2.ipkg

clean:
	idris2 --clean stg-idris2.ipkg

repl:
	idris2 --repl stg-idris2.ipkg

test1:
	./build/exec/stg-idris2 --cg stg src/HW.idr -o hw.json --dumpanf hw.anf

test2:
	./build/exec/stg-idris2 --cg stg src/HW.idr -o hello.json --dumpanf hello.anf
