idris2 = idris2

FORCE:

build: Main.idr tests.ipkg FORCE
	$(idris2) --build tests.ipkg

test: FORCE
	./build/exec/runtests `pwd`/../build/exec/stg-idris2 --timing --failure-file failures --threads 1

interactive: FORCE
	./build/exec/runtests `pwd`/../build/exec/stg-idris2 --timing --failure-file failures --threads 1 --interactive
