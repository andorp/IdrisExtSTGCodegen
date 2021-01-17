The architecture of the Idris2 compiler makes easy to implement a custom code generation back-end.

The official back-ends:

- ChezScheme
- Racket
- Gambit
- JavaScript and NodeJS
- C with reference counting

Non-official back-ends:

- Dart
- Lua

https://idris2.readthedocs.io/en/latest/back-ends/custom.html one can build its own back-end.

The way to extend Idris 2 with new back-ends is to use it as a library.
The module Idris.Driver exports the function mainWithCodegens, that takes
a list of (String, Codegen), starting idris with these codegens in addition
to the built-in ones. The first codegen in the list will be set as the default codegen.

Anyone who is interested in implementing a custom back-end needs to answer the following questions:

- Which IR should be consumed by the custom back-end?
- How to represent primitive values defined by the 'Core.TT.Constant' type?
- How to represent Algebraic Data Types?
- How to implement special values?
- Does the back-end need to associate data constructors with type constructors?
- How to implement primitive operations?
- How to implement foreign functions and FFI?
- How to compile modules?
- How to embed code snippets? Use of Elaboration?
- What should the runtime system support?

First of all. We should know that Idris2 is not an optimizing compiler. Currently its focus is
to compile dependently type functional language in a timely manner. Its main purpose is checking
if the given program is correct in a dependently typed setting and generate code in form
of a lambda-calculus like IR where higher-order functions are present. (TODO: Which type?)
Although by its other intermediate languages loosen the abstractions and make friendlier for
other code generation techniques. But it is need to be stressed that all the aggressive code
optimizations should happen in the custom back-ends. The quality and readability of the generated
back-end code are on the shoulders of the implementor of the back-end. With this in mind let's
answer the questions above.

The architecture of an Idris back-end
====================================

Idris compiles its dependently typed front-end language into a representation which is
called 'Compile.TT.Term' . This datatype has a few constructors and it represents a dependently
typed term. This 'Term' is transformed to 'Core.CompileExpr.CExp' which has a bit more constructors
and it is a very similar construct to a lambda calculus with let binding, structured tagged data
creation, primitive operations, external operations, and case expressions. The 'CExp' is closer
to code generation.

The custom code generation backend gets context of definitions, a template directory and
an an output directory, a 'Core.TT.ClosedTerm' to compile and a path to an output file.

.. code-block:: idris

   compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String)
           -> ClosedTerm -> (outfile : String) -> Core (Maybe String)
   compile defs tmpDir outputDir term file = ?

The 'ClosedTerm' is a special 'Term' where the list of the unbound variables is empty. This
technicality is not important for the code generation of the custom backend as the backend needs to
call the 'getCompileData' function which produces the 'Compiler.Common.CompileData' record.
The 'CompileData' contains a main expression that will be the entry point for the program in
as 'CExp', a list of 'Core.CompileExpr.NamedDef', a list of lambda-lifted definitions
'Compiler.LambdaLift.LiftedDef', a list of 'Compiler.ANF.ANFDef' and a list of
'Compiler.VMCode.VMDef' definitions. These definitions contains function or top-level data
definition, runtime chrashes which represent unfilled holes in idris programs, and foreign
call constructs. The job of the custom code generation backend is to transform one of the phase
encoded definitions into the intermediate representation of the code generator, which will run
optimisation and generate some form of executable. In summary the code generator has to
understand how to represent tagged data, function application even if is is partial function
application, how to handle let expressions, how to implement and invoke primitive operations,
how to handle Erased arguments, and how to do runtime crashes.
 The implementor of the custom backend should pick the closest phase of the abstraction that
the custom backend support making the transformation from one of the interternal representation
of Idris to the custom backend.
 Also the implementor should consider how to transform the simple main expression which is
represented in CExp.

Which IR should be consumed by the custom back-end?
---------------------------------------------------

Now lets turn our attention to the different IRs that Idris provides. When the 'getCompiledData'
is invoked with the Phase parameter it will produce a 'CompileData' record, which will contain
lists of top-level definitions that needs to be compiled. These are:

- NamedDef
- LiftedDef
- ANFDef
- VMDef

The question to answer here is: Which one should be picked, which ones fits to the custom back-end?
Lets see at which level what is introduced by the Idris compiler.

**NamedDef**

**LiftedDef**

**ANFDef**

**VMDef**

How to represent primitive values defined by the 'Core.TT.Constant' type?
-------------------------------------------------------------------------

After one selects which IR should be used during code generation, the next next question is to
answer how primitive types should be represented in the backend. Idris has the following kind
of primitive types:

- Int
- Integer: Arbitrary precision integer.
- Bits
- Char
- String
- Double
- World

And as Idris does pattern match on types all the primitive types has its primitive correspondent:

- IntType
- IntegerType
- BitsType
- StringType
- CharType
- DoubleType
- WorldType

How to represent these primitive types must be a well-founded design decision as it affects many
part of the code generation, such as conversion from the backend values when FFI is involved,
big part of the data during the runtime is represented in these forms. It affects the possible
optimisation techniques, and it affects the memory management and garbage collection.

In these primitive types, there are two special ones. String and World, lets zoom into them

**String**

As its name suggest this type represent a TODO.

**World**

- Boxed
- Unboxed
- Numeric types
- String

How to represent Algebraic Data Types?
--------------------------------------

How to implement special values?
--------------------------------

- Type
- Erased

Does the back-end need to associate data constructors with type constructors?
-----------------------------------------------------------------------------

- Typed
- Non-Typed

How to implement primitive operations?
--------------------------------------

How to implement foreign functions and FFI?
-------------------------------------------

How to compile modules?
-----------------------

How to embed code snippets? Use of the Elaboration?
---------------------------------------------------

What should the runtime system support?
---------------------------------------

- Memory management
- Currency primitives
