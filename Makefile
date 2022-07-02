idris2 = idris2
test = idris2/basic055/Test

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
	ext-stg-interpreter -t stg/latest.json --libhsbase-path `pwd`/libHSbase-4.14.0.0.cbits.so -p `pwd`/data/ghc-rts-base.fullpak -p `pwd`/data/idris-haskell-interface.fullpak

test-tee: FORCE
	mkdir -p stg/
	mkdir -p idris-dump/
	rm -rf stg/latest.json
	./build/exec/stg-idris2 --cg stg test/$(test).idr -o $(shell pwd)/stg/latest.json --dumpcases idris-dump/latest.cases --dumplifted idris-dump/latest.lifted --dumpanf idris-dump/latest.anf --dumpvmcode idris-dump/latest.vm --directive debug-info | tee idris-dump/latest.run
	# cat stg/latest.json | jq . > stg/latest.pretty.json
	ext-stg-interpreter -s -t stg/latest.json --libhsbase-path `pwd`/libHSbase-4.14.0.0.cbits.so -p `pwd`/data/ghc-rts-base.fullpak -p `pwd`/data/idris-haskell-interface.fullpak | tee stg/latest.stg
