
build: src stg-idris2.ipkg FORCE
	idris2 --build stg-idris2.ipkg

FORCE:

clean:
	idris2 --clean stg-idris2.ipkg
	rm -r build

repl:
	idris2 --repl stg-idris2.ipkg

test: FORCE
	./build/exec/stg-idris2 --cg stg test/Hello.idr -o $(shell pwd)/stg/hello.json --dumpanf anf/hw.anf

tests: FORCE
	./build/exec/stg-idris2 --cg stg test/HW.idr -o $(shell pwd)/stg/hw.json --dumpanf anf/hw.anf
	./build/exec/stg-idris2 --cg stg test/TPat.idr -o $(shell pwd)/stg/tpat.json --dumpanf anf/tpat.anf
	./build/exec/stg-idris2 --cg stg test/FibTest.idr -o $(shell pwd)/stg/fibtest.json --dumpanf anf/fibtest.anf
