#!/bin/sh

./build/exec/runtests `pwd`/../build/exec/stg-idris2 --timing --failure-file failures --threads 1
