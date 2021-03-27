# STG backend for Idris2

## Stage

Pre-alpha, compiles a HelloWorld

## How to build

 * Install Idris 2 (version 0.3.0-ec77ad21a)
   https://github.com/idris-lang/Idris2/blob/master/INSTALL.md
 * Install Compiler API of Idris2
 * Install Ext-STG-Interpreter andorp/idris-primops branch
   https://github.com/grin-compiler/ghc-whole-program-compiler-project/tree/andorp/idris-primops

## How to test

 * Build with `make clean && make`
 * Run test with `make test`
 * Run `ext-stg-interpreter test0.json` in the stg directory
