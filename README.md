# STG backend for Idris2

## Stage

Pre-alpha, compiles a HelloWorld

## Financial Support

<a href="https://www.patreon.com/AndOrP">
<img src="https://c5.patreon.com/external/logo/become_a_patron_button.png" width="150"/>
</a>

## How to build

 * Install Idris 2, see VERIONS file
   https://github.com/idris-lang/Idris2/blob/master/INSTALL.md
 * Install Compiler API of Idris2
 * Install Ext-STG-Interpreter andorp/idris-primops branch
   https://github.com/grin-compiler/ghc-whole-program-compiler-project/tree/andorp/idris-primops

## How to test

 * Build with `make clean && make`
 * Run test with `make test`
 * Run `ext-stg-interpreter test0.json` in the stg directory
