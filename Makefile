idris2 = idris2
# test = PrimOps/Test5
test = ffi/Test0
# test = typedd-book/chapter02/reverse/Reverse

build: src stg-idris2.ipkg FORCE
	$(idris2) --build stg-idris2.ipkg

FORCE:

watch:
	while inotifywait -e close_write -r src; do $(idris2) --typecheck stg-idris2.ipkg; done

clean:
	$(idris2) --clean stg-idris2.ipkg
	rm -r build

typecheck:
	$(idris2) --typecheck stg-idris2.ipkg

repl:
	rlwrap $(idris2) --repl stg-idris2.ipkg

test: FORCE
	mkdir -p anf
	mkdir -p stg
	mkdir -p vm
	mkdir -p anf/
	rm -rf stg/latest.json
	./build/exec/stg-idris2 --cg stg test/$(test).idr -o $(shell pwd)/stg/latest.json --dumpcases cases/latest.cases --dumplifted lifted/latest.lifted --dumpanf anf/latest.anf --dumpvmcode vm/latest.vm
	cat stg/latest.json | jq . > stg/latest.pretty.json
	ext-stg-interpreter -t stg/latest.json

test-tee: FORCE
	mkdir -p anf
	mkdir -p stg
	mkdir -p vm
	mkdir -p anf/
	rm -rf stg/latest.json
	./build/exec/stg-idris2 --cg stg test/$(test).idr -o $(shell pwd)/stg/latest.json --dumpanf anf/latest.anf --dumpvmcode vm/latest.vm --directive debug-info | tee latest.run
	cat stg/latest.json | jq . > stg/latest.pretty.json
	ext-stg-interpreter -s -t stg/latest.json | tee stg/latest.stg
