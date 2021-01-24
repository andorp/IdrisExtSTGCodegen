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
- Does the back-end need to associate data constructors with type constructors? TODO: Rename
- How to implement primitive operations?
- How to compile IR expressions?
- How to compile Definitions?
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

TODO: Mention that Idris erases type information, in the IRs as it compiles to scheme by default,
and there is no need to keep the type information around.

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
 More precisely, the World is created when the WorldVal is evaluated during the execution
of the program. This can happen the program gets initialized or when an unsafePerformIO
function is executed.

TODO: How .Type are represented?
In Scheme: #t and #f
In RefC: makeWorld() value and C type names, there is mix there.

How to represent Algebraic Data Types?
--------------------------------------

In Idris there are two different ways to define a datatype. Using the 'data' keyword or using the
'record' keyword. 'record' is used to define a named collection of fields. The 'data' is used
to define a datatype with more than one constructor. Lets see example for both:

.. code-block:: idris

   data Either a b
     = Left  a
     | Right b

.. code-block:: idris

   record Pair a b
     constructor MkPair
     fst : a
     snd : b

.. code-block:: idris

   data Fin : (n : Nat) -> Type where
     FZ : Fin (S k)
     FS : Fin k -> Fin (S k)

Both data and record is compiled to Constructors in the IR representations.

Compiling the 'Either' datatype will produce three constructor definitions in the IR:

- One for the 'Either' type itself, with the arity of two. The arity tells how many parameters
  of the constructor should have. Two here is reasonable as the original Idris 'Either' type had
  two parameters.
- One for the 'Left' constructor with arity of three. Three here is a bit surprising, as the
  constructor only have one field in Idris, but we should keep in mind the type parameters for
  the datatype too. Although the arguments associated with types can be erased in certain cases
  and they are not real part of the constructor arguments, the number of real arguments needs to
  be computed. See later in the 'compile IR expression' section.
- One for the 'Right' constructor with arity of three. Same as above.

For data constructors Idris fills out the tag field in the IR with an integer that show the order
of the constructor in the original Idris dataype. For example MyLeft gets 0 and MyRight gets 1.

Constructors can be considered structured information with a tag associated with the parameters.
The custom backend needs to decide how to represent such data. For example using SExp in a Lisp
like language, Dict in ptyhon, JSON in JavaScript etc. -- TODO check SExpr
The most important thing that these structured values are heap related values, should be
created and stored dynamically, if the there is an easy mapping in the host technology
the memory management for these values could be inherited. If not the host technology is
responsible for memory management, such as in the RefC implementation.

How to implement special values?
--------------------------------

Apart from the data constructors there are two special kind of values present in the Idris IRs.
Constructors that are created for type constructors. And values that are only part of the
computation in compile time in type elaboration.

Pattern match on types is allowed in Idris:

.. code-block::idris

   notId : {a : Type} -> a -> a
   notId {a=Int} x = x + 1
   notId x = x

Here we can pattern match on {a} and implement different behaviour for Int than the rest of the
types. This will generate an IR that will contain a Case expression with two branches, one for
the, where there is one Alt for matching the Int type constructor and a default for the non-Int
matching part of the notId function.
 This is not that special. The same mechanism needs to be used in the custom backend and the host
technology that was used for data constructors as in dependently typed languages the logic
system has no distinction at type and value level, compilation of type level terms are the same
as value level terms. This is one of the elegances of the dependently typed abstraction.

The other special value is the 'Erased'. This is generated by the Idris compiler and part of the
IR if the original value is only needed during the type elaboration process. For example:

.. code-block::idris

   data Subset : (type : Type)
              -> (pred : type -> Type)
              -> Type
     where
       Element : (value : type)
              -> (0 prf : pred value)
              -> Subset type pred

The 'prf' which is guaranteed to be erased during runtime, due to its 0 quantity.
Will be represented as 'Erased' value in the IR. The custom backend needs to represent this value
too as any other data value, as it could occur in the place of normal values. The best approach
is to implement it as a special data constructor and let the host technology provided optimisations
take care of its removal.

Does the back-end need to associate data constructors with type constructors?
-----------------------------------------------------------------------------

A very important question to answer is how to think about the set of data constructors and their
type constructors. The information of which data constructor corresponds to which type constructor
can be derived from the 'Ref Ctx' TODO. The decision made here needs to include the fact how the
case expression on structured data will be implemented. If the host technology has pattern matching
on structured data, mapping case expressions to that construct seems the obvious choice. But
the these cases probably the type constructor associated with the data constructors is needed
for the code generator of the host technology. If the host technology doesn't support pattern
matching on data constructors, it needs to approach the problem differently, for example
match on the associated tag of the data constructor inside a case/switch expression, or create a
chain of if-then-else calls.

If the data constructor association is needed it introduces a new problem, as Idris does pattern
match on the types too, implementation on pattern matching on types, shouldn't be different from
the implementation of pattern match on data. Because of that reason the custom backend
needs to create a data-type in the host technology that collects all the datatypes defined
in the Idris program and also present in the IR definitions as Constructors that
represents types. For the collected type constructors the backend should create a datatype
in the host technology which summarizes them. With this host datatype it will be available
to implement a case pattern match on the types of the Idris program

How to implement primitive operations?
--------------------------------------

Primitive operations are defined in Idris compiler with the Core.TT.PrimFn. The constructors
of this datatype represent the primitive operations that the custom backend needs to implement.
These primitive operations can be grouped as:

- Arithmetic operations (Add, Sub, Mul, Div, Mod, Neg)
- Bit operations (ShiftL, ShiftR, BAnd, BOr, BXor)
- Comparing values (LT, LTE, EQ, GTE, GT)
- String operations (Length, Head, Tail, Index, Cons, Append, Reverse, Substr)
- Double precision floating point operations (Exp, Log, Sin, Cos, Tan, ASin, ACos, ATan, Sqrt, Floor, Ceiling)
- Casting of numeric and string values
- BelieveMe: This primitive helps the type checker. When the type checker sees the 'beleive_me'
  function call, it will cast type 'a' to type 'b'. For details see below.
- Crash: TODO With 2 parameter

BeleiveMe: The 'believe_me' is defined in the Builtins module. But what does this mean for the
custom backend? As Idris assumes that the backend representation of the data is not strongly
typed and any datatype has the same kind of representation. This could introduce a constraint on
the representation of the primitive and constructor represented datatypes. One possible solution
is that the custom backend should represent primitive datatypes the same way as constructors,
but the tags are special ones. For example: IdrisInt.

TODO: Check how Official backends represents such data.
RefC: Boxes the primitives, which makes them easy to put on the heap.
Scheme: Prints the values as Scheme literals when the value comes from a Constant value.

How to compile Top-Level definitions?
-------------------------------------

As mentioned earlier, Idris has 4 different IRs that is available in the 'CompileData' record:
Named, LambdaLifted, ANF, and VMCode. When assembling the 'CompileData' we have to tell the
Idris compiler to which level we are interested in. The 'CompileData' contains lists of
definitions, that can be considered as top level definitions that the custom backend need
to generate functions for. These definitions not always contain an actual function definitions,
but sometimes top-level data, or crash instructions.

There are four types of top-level definitions that the code generation backend needs to support:

- Function
- Constructor
- Foreign call
- Error


**Function** contains and IR expression which needs to be compiled to the expressions of the
host technology. These expressions are lambda calculus like expressions, and the custom backend
needs to decide how to represent them.

**Constructor** represent a data or a type consturctor in the frontend language, and they should
be implemented as functions in the backends, which will create the corresponding construction
in the custom backend. The decisions taken in answering the 'How to represent Algebraic Data Types?'
question plays a role here.

Top-level **foreign call** defines an entry point for calling functions implemented outside the
Idris program under compile. The Foreign construction contains a list of String which
are the snippets defined by the programmer and foreign type information of the arguments
and return type of the foreign function. Formally a (css : List String), (fargs : List CFType),
and (ret : CFType). Using this information the custom backend needs to generate code in the
host technology which could invoke the function call in the host technology, wrapping and
unwrapping the Idris values (which are represented as CFType) between the runtime for the Idris
in the host techniology and the foreign function.
 The CFType contains the following definitions, many of them one-to-one mapping from the
corresponding primitive type, but some of them needs explanation.
 At this point we should mention that the design decision taken
about how to represent primitive types in the host technology also has effects on the design
of hot to do the interfacing with foreign defined functions.

- CFUnit
- CFInt
- CFUnsigned8
- CFUnsigned16
- CFUnsigned32
- CFUnsigned64
- CFString
- CFDouble
- CFChar
- CFFun : CFType -> CFType -> CFType
  Callbacks can be registered in the host technology via the parameters that have CFFun type.
  The backend should be capable of embed functions that are defined in Idris side and compiled
  to the host technology. If the custom backend supports higher order functions that is a good
  candidate to use to implement the support for this kind of FFI type. An example of this
  can be found in the Callbacks section of FFI as in the 'applyFnIO' section. TODO
- CFIORes : CFType -> CFType
  Any PrimIO defined computation will have this extra layer. Because of this pure and IO functions
  in the host technology should be well-thought. Pure functions shouldn't have any IO observable IO
  effect on the program state in the Host technology.
   Important thing to note, that IORes is also used when callback functions are registered in the
  host technology.
- CFWorld : Represend the current state of the world. This should mean a token that are passed
  around between function calls. The implementation of the World value should contain backend
  specific values information about the state of the Idris runtime.
- CFStruct : String -> List (String, CFType) -> CFType
  The foreign type associated with the 'System.FFI.Struct'. It represents a C like structure
  in the custom backend. prim__getField prim__setField primitives should be implemented
  to support this CFType.
- CFUser : Name -> List CFType -> CFType
  Types defined with [external] are represented with CFUser. For example
  'data MyType : Type where [external]' will be represented as
  'CFUser Module.MyType []'
- CFBuffer - Foreign type defined for Data.Buffer as in data Buffer : Type where [external]
  Although this is an external type, the Idris builds on a random access buffer. It is expected
  from the custom backend to provide an appropiate implementation for this external type out
  of the box.
- CFPtr The 'Ptr t' and 'AnyPtr' are compiled to CFPtr. Any complex structured data that can not
  be represented as a simple primitive can use this CFPtr to keep track where the value is used.
  In Idris 'Ptr t' is defined as external type.
- CFGCPtr The 'GCPtr t' and 'GCAnyPtr' are compiled to CFGCPtr. GCPtr has a special rule, it born
  from a Ptr value calling the 'onCollect' function. The onCollect attaches a finalizer for the Ptr
  which should run when the pointer happens to be freed by the Garbage Collector of the Idris
  runtime. If there is no garbage collector, like in RefC backend the finalizer should be called
  when the allocated memory for the value represented by the GCPtr gets freed.

Top-level **error** definition represents holes in Idris programs. This is necessary because
Idris compiles non-complete programs. Lets see the following example:

.. code-block:: idris

   missing : Int
   missing = ?someting

   main : IO ()
   main = printLn missing

Pragmatic (dependently typed) programming requires working on parts of the program,
without actually writing all the program in one go. Different programming languages
have different approaches for the pragmatic aspects of programming. For example in
Java it is customary to throw RuntimeExceptions, in Haskell use undefined of error.
 In Idris the partial program approach is a tool. The developer may want to define
parts of the program using holes. Identifiers which starts with the '?' character
are consider holes. They play a big part in the development cycle of an Idris
program. But turn our attention again to code generation.
 In Idris holes are compiled to the Crash operation which should halt the program
execution. Meanwhile this is desired attribute during the development phase of
the program, it is unfortunate to have runtime exceptions lurking around in the
program that is considered done. Having holes formally distinguished from runtime
exceptions makes explicit that the program is not complete nor considered to be
released into production.

How to compile IR expressions?
------------------------------

The custom backend should decide from which form on the intermediate representation
should transform into the expressions and functions of the host technology. Definitions
in ANF and Lifted are represented as a tree like expression, where control flow is based
on the 'Let' and 'Case' expressions.

There are two types of case expressions, one for matching and branching on primitive
values such as Int, and the second one is matching and branching on constructor values.
The two types of case expressions will have two different representetion for alternatives
of the cases. These are: ConCase and ConstCase. As one can suspect ConCase is for matching
the constructor values and ConstCase is for matching the constant values.
The matching on constructor values is based on matching on the tag of the constructor
and binding the values of parameter to variables in the body of the matching branch.
Such as 'Cons x xs =>'. The matching and branching should be implemented in the host technology
using its branching constructions.

There are two ways of creating a value. If the value is a primitive value there is
PrimVal construction which should create some kind of constant in the host technology. Design
decisions made at the 'How to represent primitive values?' section will have consequences here too.
For thestructured value; the Con construction is there, which should be compiled to a function
in the host technology which creates a dynamic like value. Design decisions made for
'How to represent consturctor values?' will have effect here.

There are four types of function calls: Function application where all the arguments
have values associated with them. Under Appliaction where some of the arguments have
values associated with them, but some of them are still unassociated. Calling a primitive
operation with all its arguments associated. The primitive operation is part of the PrimFn
construction. And the last one is to calling a foreign function which is referred
by its name.

As the ANF and Lifted has UnderApp construction, that means the custom backend needs to
support partial application of functions and creating some kind of closures in the
host technology. This is not a problem with backends like Scheme we get the partial application
of a function as an already existing tool, but if the host technology does not have this
tool in its toolbox, the custom backend needs to simulate closures. One possibly simple
solution to this is to record the partially applied values in a special object for the
closure and evaluate it when it has all the necessary arguments applied to it. The same
approach is needed if the VMCode IR was chosen for code generation.

There is a Let construction in the ANF and Lifted IR. To have access to the value that was
bind to the variable in the let expression, the AV or the Local must be used. For these
the custom backend needs to implement assignment like structures. Both of AV and Local
referred values may contain closures.
The difference between the Lifted ANF is that meanwhile in Lifted Local variables
can be referenced explicitly and the arguments of function are part of the type of
the Lifted 'data Lifted : List Name -> Type', in ANF the variables are addressed
via the 'data AVar = ALocal Int | ANull'. The ANull value refers to an erased variable
and it should represented what was decided in the how to represent Erased values.

Both ANF and Lifted contain an Erased and Crash operations. Erased creates a special
value, which only was significant and compiletime and it shouldn't store any information
at runtime.
 The Crash represents an operation of system crash. When its called, the execution of
the Idris program should be halt. Crashes are compiled for holes in programs.

The third approach for expression is the approach can be found in the VMDef. In the VMDef
which meant to be the closest to machine code the abstraction is formulated around
the list of instructions and registers. There is no Let expression at this level, it
is replaces by ASSIGN. Case expressions for constructor data does not bind variables,
an extra operation is introduced PROJECT, which extracts information of the structured data.
There is no App and UnderApp is replaced by APPLY which applies only one value and creates
a closure from the application. For erased values the operation NULL assign an empty/null
value for the register.

How to implement foreign functions and FFI?
-------------------------------------------

How to compile modules?
-----------------------

The Idris compiler generates intermediate files for modules, the content of the files are not the
Lifted, ANF, nor VMCode. Because of this, when the compilation pipeline enters the stage at code
generation all the information will be in one instance of the CompileData record and the custom
code generator backend can process them as it would see the whole program at this stage.
 Maybe the custom backend wants to introduce some hiearchy for the functions in different
namespaces and organize some module structure to let the host technology process the bits and pieces
in different sized chunks, but this feature is not in scope of the Idris compiler.

How to embed code snippets? Use of the Elaboration?
---------------------------------------------------

What should the runtime system support?
---------------------------------------

- Memory management
- Currency primitives
