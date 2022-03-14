idris2 = idris2
test = Test5

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
	rm -rf stg/$(test).json
	./build/exec/stg-idris2 --cg stg test/$(test).idr -o $(shell pwd)/stg/$(test).json --dumpanf anf/$(test).anf --dumpvmcode vm/$(test).vm | tee $(test).run
	cat stg/$(test).json | jq . > stg/$(test).pretty.json
#	ext-stg-interpreter stg/$(test).json | tee stg/$(test).stg
	ext-stg-interpreter stg/$(test).json
