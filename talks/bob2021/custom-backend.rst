This document addresses the details how to implement a custom code generation for the Idris compiler.

This part has no insights about how to implement the dependently typed bits.
For that part of the compiler Edwin Brady gave lectures at SPLV'20 which are available here:
https://www.youtube.com/playlist?list=PLmYPUe8PWHKqBRJfwBr4qga7WIs7r60Ql

The architecture of the Idris2 compiler makes easy to implement a custom code generation back-end.

The official back-ends are:

- ChezScheme
- Racket
- Gambit
- JavaScript and NodeJS
- C with reference counting

Non-official back-ends are:

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

 As Idris does not focuses on memory management and threading. The custom backend
should model these concept for the program that is compiled from the Idris user facing source code.
One possible approach is to reuse as much as possible from the host/custom backend and/or implement
a runtime that is capable of handling the memory management and threading.

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

As its name suggest this type represent a string of characters. As mentioned in
https://idris2.readthedocs.io/en/latest/ffi/ffi.html#primitive-ffi-types 'Primitive FFI Types'
in Idris Strings are encoded as UTF-8, it is not always clear who is responsible for freeing
a String created by other component than the Idris runtime. Also in Idris String will always have
value. This creates constraints on the possible representations of the Strings in the custom
backend, diverging from the Idris representation is not a good idea. The best approach here
is to build a conversion layer between the String representation of the custom backend and the
runtime which is implemented for Idris.

**World**

In pure function programming there is a need represent somehow causality. To maintain order of the
execution, the sequence of commands a token must be used to chain function calls. This is abstract
notion of where the state of the world should be represented. For example this
information could be the list of Data.IORefs that are created during the running of an Idris program.

The World value in the Idris programs are accessed via the 'primIO' construction which
leads us to the PrimIO module. Lets see the relevant snippets:

.. code-block:: idris

   data IORes : Type -> Type where
        MkIORes : (result : a) -> (1 x : %World) -> IORes a

   fromPrim : (1 fn : (1 x : %World) -> IORes a) -> IO a
   fromPrim op = MkIO op

   primIO : HasIO io => (1 fn : (1 x : %World) -> IORes a) -> io a
   primIO op = liftIO (fromPrim op)

TODO: How world is created???
The world value is referenced as '%World' in Idris. It is created by the runtime when
the program starts. Its content is changed by the custom runtime. As the code snippets shows
the %World must be used linearly, which is a strong guarantee for the runtime system.

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
