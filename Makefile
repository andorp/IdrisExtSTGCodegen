idris2 = idris2
test = ffi/test5/Test5

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
	mkdir -p stg/
	mkdir -p idris-dump/
	rm -rf stg/latest.json
	./build/exec/stg-idris2 --cg stg test/$(test).idr -o $(shell pwd)/stg/latest.json --dumpcases idris-dump/latest.cases --dumplifted idris-dump/latest.lifted --dumpanf idris-dump/latest.anf --dumpvmcode idris-dump/latest.vm
	ext-stg-interpreter -t stg/latest.json

test-tee: FORCE
	mkdir -p stg/
	mkdir -p idris-dump/
	rm -rf stg/latest.json
	./build/exec/stg-idris2 --cg stg test/$(test).idr -o $(shell pwd)/stg/latest.json --dumpcases idris-dump/latest.cases --dumplifted idris-dump/latest.lifted --dumpanf idris-dump/latest.anf --dumpvmcode idris-dump/latest.vm --directive debug-info | tee idris-dump/latest.run
	# cat stg/latest.json | jq . > stg/latest.pretty.json
	ext-stg-interpreter -s -t stg/latest.json | tee stg/latest.stg
