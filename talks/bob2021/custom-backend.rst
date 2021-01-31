This document addresses the details how to implement a custom code generation for the Idris compiler.

This part has no insights about how to implement the dependently typed bits.
For that part of the compiler Edwin Brady gave lectures at SPLV20_ which are available here:


The architecture of the Idris2 compiler makes easy to implement a custom code generation back-end.

The official back-ends are:

- ChezScheme
- Racket
- Gambit
- JavaScript and NodeJS
- C with reference counting

Non-official back-ends are:

- Dart_ https://github.com/bamboo/idris2dart
- Lua_ https://github.com/Russoul/idris2-lua
- Erlang_ https://github.com/idris-community/Idris2-Erlang

`Custom backend <https://idris2.readthedocs.io/en/latest/backends/custom.html>`_
one can build its own back-end.

The way to extend Idris with new back-ends is to use it as a library.
The module Idris.Driver exports the function mainWithCodegens, that takes
a list of (String, Codegen), starting idris with these codegens in addition
to the built-in ones. The first codegen in the list will be set as the default codegen.

Anyone who is interested in implementing a custom back-end needs to answer the following questions:

- Which IR should be consumed by the custom back-end?
- How to represent primitive values defined by the ``Core.TT.Constant`` type?
- How to represent Algebraic Data Types?
- How to implement special values?
- Association between data and type constructors.
- How to implement primitive operations?
- How to compile IR expressions?
- How to compile Definitions?
- How to implement Foreign Function Interface?
- How to compile modules?
- How to embed code snippets?
- What should the runtime system support?

First of all. We should know that Idris2 is not an optimizing compiler. Currently its focus is
to compile dependently type functional language in a timely manner. Its main purpose is checking
if the given program is correct in a dependently typed setting and generate code in form
of a lambda-calculus like IR where higher-order functions are present. (TODO: Which type?)
Although by its other intermediate languages loosen the abstractions and make friendlier for
other code generation techniques. It should be stressed that all the aggressive code
optimizations should happen in the custom back-ends. The quality and readability of the generated
back-end code is on the shoulders of the implementor of the back-end. With this in mind lets
answer the questions above.

TODO: Mention that Idris erases type information, in the IRs as it compiles to scheme by default,
and there is no need to keep the type information around.

The architecture of an Idris back-end
=====================================

Idris compiles its dependently typed front-end language into a representation which is
called ``Compile.TT.Term`` . This data type has a few constructors and it represents a dependently
typed term. This ``Term`` is transformed to ``Core.CompileExpr.CExp`` which has more constructors
then ``Term`` and it is a very similar construct to a lambda calculus with let bindings, structured and tagged data
representation, primitive operations, external operations, and case expressions. The ``CExp`` is closer
to code generation.

The custom code generation back-end gets context of definitions, a template directory and
an an output directory, a ``Core.TT.ClosedTerm`` to compile and a path to an output file.

.. code-block:: idris

   compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String)
           -> ClosedTerm -> (outfile : String) -> Core (Maybe String)
   compile defs tmpDir outputDir term file = ?

The ``ClosedTerm`` is a special ``Term`` where the list of the unbound variables is empty. This
technicality is not important for the code generation of the custom back-end as the back-end needs to
call the ``getCompileData`` function which produces the ``Compiler.Common.CompileData`` record.

The ``CompileData`` contains:
- A main expression that will be the entry point for the program in ``CExp``
- A list of ``Core.CompileExpr.NamedDef``
- A list of lambda-lifted definitions ``Compiler.LambdaLift.LiftedDef``
- A list of ``Compiler.ANF.ANFDef``
- A list of ``Compiler.VMCode.VMDef`` definitions

These lists contain
- Functions
- Top-level data definitions
- Runtime crashes which represent unfilled holes in idris programs
- Foreign call constructs.

The job of the custom code generation back-end is to transform one of the phase
encoded definitions (NamedDef, LiftedDef, CExp, ANF, VM) into the intermediate representation
of the code generator, which will run optimizations and generate some form of executable.
In summary the code generator has to understand how to represent tagged data, function application
even if it is partial function application, how to handle let expressions, how to implement and
invoke primitive operations, how to handle Erased arguments, and how to do runtime crashes.

The implementor of the custom back-end should pick the closest phase of the abstraction that
the custom back-end support making the transformation from one of the intermediate representation
of Idris to the custom back-end.
Also the implementor should consider how to transform the simple main expression which is
represented in CExp.
As Idris does not focuses on memory management and threading. The custom back-end
should model these concept for the program that is compiled.
One possible approach is to reuse as much as possible from the host/custom back-end and/or implement
a runtime that is capable of handling the memory management and threading.

Which IR should be consumed by the custom back-end?
---------------------------------------------------

Now lets turn our attention to the different IRs that Idris provides. When the ``getCompiledData``
is invoked with the Phase parameter it will produce a ``CompileData`` record, which will contain
lists of top-level definitions that needs to be compiled. These are:

- NamedDef
- LiftedDef
- ANFDef
- VMDef

The question to answer here is: Which one should be picked? Which one fits to the custom back-end?
TODO: Lets see at which level what kind of construction is introduced by the Idris compiler.

How to represent primitive values defined by the ``Core.TT.Constant`` type?
-------------------------------------------------------------------------

After one selects which IR should be used during code generation, the next question is to
answer how primitive types should be represented in the back-end. Idris has the following
primitive types:

- Int
- Integer: Arbitrary precision integer.
- Bits
- Char
- String
- Double
- World

And as Idris does pattern match on types all the primitive types has its primitive correspondent
for describing a type:

- IntType
- IntegerType
- BitsType
- StringType
- CharType
- DoubleType
- WorldType

How to represent these primitive types must be a well-founded design decision as it affects many
part of the code generation, such as conversion from the back-end values when FFI is involved,
big part of the data during the runtime is represented in these forms. It affects the possible
optimisation techniques, and it affects the memory management and garbage collection.

In these primitive types, there are two special ones. String and World.

**String**

As its name suggest this type represent a string of characters. As mentioned in
`Primitive FFI Types <https://idris2.readthedocs.io/en/latest/ffi/ffi.html#primitive-ffi-types>`_
in Idris Strings are encoded as UTF-8, it is not always clear who is responsible for freeing
a String created by other component than the Idris runtime. Also in Idris String will always have
value. This creates constraints on the possible representations of the Strings in the custom
back-end, diverging from the Idris representation is not a good idea. The best approach here
is to build a conversion layer between the String representation of the custom back-end and the
runtime which is implemented for Idris.

**World**

In pure functional programming there is a need represent causality somehow. To maintain order of the
execution, the sequence of commands a token must be used to chain function calls of IO. This is an abstract
notion about the state of the world. For example this
information could be the information that the runtime needs for bookkeeping for the running program.

The World value in Idris programs are accessed via the ``primIO`` construction which
leads us to the PrimIO module. Lets see the relevant snippets:

.. code-block:: idris

   data IORes : Type -> Type where
        MkIORes : (result : a) -> (1 x : %World) -> IORes a

   fromPrim : (1 fn : (1 x : %World) -> IORes a) -> IO a
   fromPrim op = MkIO op

   primIO : HasIO io => (1 fn : (1 x : %World) -> IORes a) -> io a
   primIO op = liftIO (fromPrim op)

The world value is referenced as ``%World`` in Idris. It is created by the runtime when
the program starts. Its content is changed by the custom runtime. As the code snippets shows
the ``%World`` must be used linearly, which is a strong guarantee for the runtime system.
More precisely, the World is created when the WorldVal is evaluated during the execution
of the program. This can happen the program gets initialized or when an ``unsafePerformIO``
function is executed.

How to represent Algebraic Data Types?
--------------------------------------

In Idris there are two different ways to define a data type. Using the ``data`` keyword or using the
``record`` keyword. ``record`` is used to define a named collection of fields. The ``data`` is used
to define a data type with more than one constructor. Lets see example for both:

.. code-block:: idris

   data Either a b
     = Left  a
     | Right b

.. code-block:: idris

   record Pair a b
     constructor MkPair
     fst : a
     snd : b

Also an example for a data type which is called an indexed data type. Its parameter is another
data type which is a useful construction in the dependently typed setting:

.. code-block:: idris

   data Fin : (n : Nat) -> Type where
     FZ : Fin (S k)
     FS : Fin k -> Fin (S k)

Both data and record is compiled to Constructors in the IR representations. Constructor such as
``Core.CompileExpr.CExp.CCon`` and ``Core.CompileExpr.CDef.MkCon``.

Compiling the ``Either`` data type will produce three constructor definitions in the IR:

- One for the ``Either`` type itself, with the arity of two. The arity tells how many parameters
  of the constructor should have. Two here is reasonable as the original Idris ``Either`` type has
  two parameters.
- One for the ``Left`` constructor with arity of three. Three here is a bit surprising, as the
  constructor only have one field in Idris, but we should keep in mind the type parameters for
  the data type too. Although the arguments associated with types can be erased in certain cases
  and they are not real part of the constructor arguments, the number of real arguments needs to
  be computed. See later in the 'How to compile IR expressions' section. TODO: Link
- One for the ``Right`` constructor with arity of three. Same as above.

In the IR the constructors have unique names and for data constructors Idris fills out the tag field
with an integer that show the order of the constructor in the original Idris data type.
In the Either example above Left gets tag 0 and Right gets tag 1.

Constructors can be considered structured information with a name associated with the parameters.
The custom back-end needs to decide how to represent such data. For example using SExp in a Lisp
like language, Dict in Python, JSON in JavaScript etc. -- TODO check SExpr
The most important thing that these structured values are heap related values, should be
created and stored dynamically. If there is an easy mapping in the host technology,
the memory management for these values could be inherited. If not the host technology is
responsible for implementing an appropriate memory management. For example the RefC
back-end implements its own memory management based on reference counting.

How to implement special values?
--------------------------------

Apart from the data constructors there are two special kind of values present in the Idris IRs.
Constructors that are created for Idris types and values that are only part of the
computation in compile time and will be erased from the IR representation.

Pattern match on types is allowed in Idris:

.. code-block:: idris

   notId : {a : Type} -> a -> a
   notId {a=Int} x = x + 1
   notId x = x

Here we can pattern match on {a} and implement different behaviour for Int than the rest of the
types. This will generate an IR that will contain a Case expression with two branches,
one Alt for matching the Int type constructor and a default for the non-Int matching part of the
notId function.

This is not that special. The same mechanism needs to be used in the custom back-end and the host
technology that is used for data constructors. The reason for using the same approach is; in
dependently typed languages the logic system has no distinction at type and value level,
compilation of type level terms are the same as value level terms. This an elegance of the
dependently typed abstraction.

The other special value is the ``Erased``. This is generated by the Idris compiler and part of the
IR if the original value is only needed during the type elaboration process. For example:

.. code-block:: idris

   data Subset : (type : Type)
              -> (pred : type -> Type)
              -> Type
     where
       Element : (value : type)
              -> (0 prf : pred value)
              -> Subset type pred

The ``prf`` which is guaranteed to be erased during runtime, due to its 0 quantity.
Will be represented as ``Erased`` value in the IR. The custom back-end needs to represent this value
too as any other data value, as it could occur in the place of normal values. The best approach
is to implement it as a special data constructor and let the host technology provided optimizations
take care of its removal.

Association between data and type constructors.
-----------------------------------------------

A very important question to answer is how to think about the set of data constructors and their
type constructors. The information of which data constructor corresponds to which type constructor
can be derived from the ``Ref Ctx``. See the code snippet below.

.. code-block:: idris

  Core.Context.Def
  TCon : (tag : Int) -> (arity : Nat) ->
         (parampos : List Nat) -> -- parameters
         (detpos : List Nat) -> -- determining arguments
         (flags : TypeFlags) -> -- should 'auto' implicits check
         (mutwith : List Name) ->
         (datacons : List Name) ->
         (detagabbleBy : Maybe (List Nat)) ->
         Def

The decision made here needs to include the fact how the
case expression on structured data will be implemented. If the host technology has pattern matching
on structured data, mapping case expressions to that construct seems the obvious choice. But
the these cases probably the type constructor associated with the data constructors is needed
for the code generator of the host technology. If the host technology doesn't support pattern
matching on data constructors, it needs to approach the problem differently, for example
match on the associated tag of the data constructor inside a case/switch expression, or create a
chain of if-then-else calls.

If the data constructor association is needed, a new problem is introduced. Idris does pattern
match on types too, implementation on pattern matching on types, shouldn't be different from
the implementation of pattern match on data. Because of that reason the custom back-end
needs to create a data type in the host technology that collects all the data types defined
in the Idris program and also present in the IR definitions as Constructors that
represents types. For the collected type constructors the back-end should create a data type
in the host technology which summarizes them. With this host data type it will be available
to implement a case pattern match on the types of the Idris program

How to implement primitive operations?
--------------------------------------

Primitive operations are defined in Idris compiler by Core.TT.PrimFn. The constructors
of this data type represent the primitive operations that the custom back-end needs to implement.
These primitive operations can be grouped as:

- Arithmetic operations (Add, Sub, Mul, Div, Mod, Neg)
- Bit operations (ShiftL, ShiftR, BAnd, BOr, BXor)
- Comparing values (LT, LTE, EQ, GTE, GT)
- String operations (Length, Head, Tail, Index, Cons, Append, Reverse, Substr)
- Double precision floating point operations (Exp, Log, Sin, Cos, Tan, ASin, ACos, ATan, Sqrt, Floor, Ceiling)
- Casting of numeric and string values
- BelieveMe: This primitive helps the type checker. When the type checker sees the ``beleive_me``
  function call, it will cast type ``a`` to type ``b``. For details see below.
- Crash: The first parameter of the crash is a type, the second is a string that represents
  the error message.

BeleiveMe: The ``believe_me`` is defined in the Builtins module. What does this mean for the
custom back-end? As Idris assumes that the back-end representation of the data is not strongly
typed and any data type has the same kind of representation. This could introduce a constraint on
the representation of the primitive and constructor represented data types. One possible solution
is that the custom back-end should represent primitive data types the same way as constructors,
but the tags are special ones. For example: IdrisInt. This is called boxing.
The ``believe_me`` construction can get data types that are defined by the ``[external]`` definition.
Which also exposes a restriction on the FFI data types. The [external] ones will be described by
the CFUser FFI type description, and that description should use the same representation than any
other Idris type in the back-end.

Official backends represents primitive data types as boxed ones.
- RefC: Boxes the primitives, which makes them easy to put on the heap.
- Scheme: Prints the values as Scheme literals when the value comes from a Constant value.

How to compile Top-Level definitions?
-------------------------------------

As mentioned earlier, Idris has 4 different IRs that is available in the ``CompileData`` record:
Named, LambdaLifted, ANF, and VMCode. When assembling the ``CompileData`` we have to tell the
Idris compiler which level we are interested in. The ``CompileData`` contains lists of
definitions, that can be considered as top level definitions that the custom back-end need
to generate functions for. These definitions not always contain an actual function definitions,
but sometimes top-level data creation, or crash instructions.

There are four types of top-level definitions that the code generation back-end needs to support:

- Function
- Constructor
- Foreign call
- Error


**Function** contains and IR expression which needs to be compiled to the expressions of the
host technology. These expressions are lambda calculus like expressions, and the custom back-end
needs to decide how to represent them.

**Constructor** represent a data or a type constructor in the front-end language, and it should
be implemented as function in the back-ends. Function which will create the corresponding
data construction in the custom back-end. The decisions taken in answering the
'How to represent Algebraic Data Types?' question plays a role here.

Top-level **foreign call** defines an entry point for calling functions implemented outside the
Idris program under compile. The Foreign construction contains a list of String which
are the snippets defined by the programmer and foreign type information of the arguments
and return type of the foreign function. Formally a ``(css : List String)``, ``(fargs : List CFType)``,
and ``(ret : CFType)``. Using this information the custom back-end needs to generate code in the
host technology which could invoke the function call in the host technology, wrapping and
unwrapping the Idris values (which are represented as CFType) between the runtime for the Idris
in the host technology and the foreign function. More on this in the 'How to do FFI TODO' section.

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
parts of the program using holes. Identifiers which starts with the ``?`` character
are considered as holes. They play a big part in the development cycle of an Idris
program. But turn our attention again to code generation.

In Idris holes are compiled with the Crash operation which should halt the program
execution. Meanwhile this is desired attribute during the development phase of
the program, it is unfortunate to have potential runtime exceptions lurking around in the
program that is considered done. Having holes formally distinguished from runtime
exceptions makes explicit that the program is not complete nor considered to be
released into production.

How to compile IR expressions?
------------------------------

The custom back-end should decide from which form on the intermediate representation
is used for transforming. The result of the transformation should be expressions
and functions of the host technology. Definitions in ANF and Lifted are represented as a tree
like expression, where control flow is based on the ``Let`` and ``Case`` expressions.

There are two types of case expressions, one for matching and branching on primitive
values such as Int, and the second one is matching and branching on constructor values.
The two types of case expressions will have two different representation for alternatives
of the cases. These are: ``ConCase`` and ``ConstCase``. ``ConCase`` is for matching
the constructor values and ``ConstCase`` is for matching the constant values.
The matching on constructor values is based on matching on the name of the constructor
and binding the values of parameter to variables in the body of the matching branch.
Such as ``Cons x xs =>``. The matching and branching should be implemented in the host technology
using its branching constructions, for example switch expressions, case with pattern matching,
or if-then-else chains.

There are two ways of creating a value. If the value is a primitive value there is
PrimVal construction which should create some kind of constant in the host technology. Design
decisions made at the 'How to represent primitive values?' section will have consequences here too.
For the structured value; the Con construction is there. It should be compiled to a function
in the host technology which creates a dynamic like value. Design decisions made for
'How to represent constructor values?' will have effect here.

There are four types of function calls:
- Function application where all the arguments have values associated with them.
- Under-application where some of the arguments have values associated with them, but some of them are still unassociated.
- Calling a primitive operation with all its arguments associated. The primitive operation is part of the PrimFn construction.
- Calling a foreign function which is referred by its name.

The ANF and Lifted have UnderApp construction, meaning the custom back-end needs to
support partial application of functions and creating some kind of closures in the
host technology. This is not a problem with back-ends like Scheme we get the partial application
of a function for free, but if the host technology does not have this
tool in its toolbox, the custom back-end needs to simulate closures. One possibly simple
solution to this is to record the partially applied values in a special object for the
closure and evaluate it when it has all the necessary arguments applied to it. The same
approach is needed if the VMCode IR was chosen for code generation.

There is a Let construction in the ANF and Lifted IR. To have access to the value that was
bind to the variable in the let expression, the AV or the Local must be used. For these
the custom back-end needs to implement assignment like structures. Both of AV and Local
referred values may contain closures.
The difference between the Lifted ANF is that meanwhile in Lifted Local variables
can be referenced explicitly and the arguments of function are part of the type of
the Lifted ``data Lifted : List Name -> Type``, in ANF the variables are addressed
via the ``data AVar = ALocal Int | ANull``. The ANull value refers to an erased variable
and it should represented what was decided in the how to represent Erased values.

Both ANF and Lifted contain an Erased and Crash operations. Erased creates a special
value, which only was significant and compile time and it shouldn't store any information
at runtime.

The Crash represents an operation of system crash. When its called, the execution of
the Idris program should be halt. Crashes are compiled for holes in programs.

The third approach for expression is the approach can be found in the VMDef. In the VMDef
which meant to be the closest to machine code the abstraction is formulated around
the list of instructions and registers. There is no Let expression at this level, it
is replaces by ``ASSIGN``. Case expressions for constructor data does not bind variables,
an extra operation is introduced ``PROJECT``, which extracts information of the structured data.
There is no App and UnderApp. Both are replaced by APPLY which applies only one value and creates
a closure from the application. For erased values the operation ``NULL`` assign an empty/null
value for the register.

When pattern matching binds variables in alternatives of constructor case expressions the
number of arguments are different from the arity of the constructor defined in top-level
definitions and in the ``GlobalDef``. This is because Idris keeps around all the arguments,
but the code generator for the alternatives removes the ones which are marked for deletion,
for such arguments the code generator of the custom back-end needs to remove the erased
arguments for the constructor implementation. As erased arguments don't hold any
variables and the arities of alternatives and constructors won't match up.
In ``GlobalDef`` the ``earseArg`` contains this information, which can be used to extract the
number of arguments which needs to be kept around.

How to implement Foreign Function Interface?
--------------------------------------------

Foreign Function Interface plays a big role in running Idris programs. The primitive operations
which are mentioned above are functions for manipulating values and those functions aren't for
complex interaction with the runtime system. Other functionality, which is part of the prelude/base,
can be think of abstract types via external and foreign
functions around them. The responsibility of the custom back-end and the host technology is
to represent these computations the operationally correct way. Originally Idris had an official
C back-end implementation. This is different for the Idris, because currently it has
an official Scheme and JavaScript back-end. Despite that the names in the types for the FFI stayed
the same with the C prefix.
The ``Core.CompileExpr.CFType`` contains the following definitions, many of them one-to-one mapping
from the corresponding primitive type, but some of them needs explanation.
At this point we should mention that the design decision taken
about how to represent primitive types in the host technology also has effects on the design
of how to do the interfacing with foreign defined functions.

The foreign types are:

- CFUnit
- CFInt
- CFUnsigned8
- CFUnsigned16
- CFUnsigned32
- CFUnsigned64
- CFString
- CFDouble
- CFChar
- CFFun ``CFType -> CFType -> CFType`` Callbacks can be registered in the host technology via the parameters that have CFFun type.
  The back-end should be capable of embed functions that are defined in Idris side and compiled
  to the host technology. If the custom back-end supports higher order functions that is a good
  candidate to use to implement the support for this kind of FFI type. An example of this
  can be found in the Callbacks section of FFI as in the ``applyFnIO`` section. TODO
- CFIORes ``CFType -> CFType`` Any PrimIO defined computation will have this extra layer. Pure functions shouldn't have any
  observable IO effect on the program state in the host technology implemented runtime.
  NOTE: IORes is also used when callback functions are registered in the
  host technology.
- CFWorld Represents the current state of the world. This should mean a token that are passed
  around between function calls. The implementation of the World value should contain back-end
  specific values information about the state of the Idris runtime.
- CFStruct ``String -> List (String, CFType) -> CFType`` The foreign type associated with the ``System.FFI.Struct``. It represents a C like structure
  in the custom back-end. prim__getField prim__setField primitives should be implemented
  to support this CFType.
- CFUser ``Name -> List CFType -> CFType``
  Types defined with [external] are represented with CFUser. For example
  ``data MyType : Type where [external]`` will be represented as
  ``CFUser Module.MyType []``
- CFBuffer - Foreign type defined for Data.Buffer as in data Buffer : Type where [external]
  Although this is an external type, the Idris builds on a random access buffer. It is expected
  from the custom back-end to provide an appropriate implementation for this external type.
- CFPtr The ``Ptr t`` and ``AnyPtr`` are compiled to CFPtr. Any complex structured data that can not
  be represented as a simple primitive can use this CFPtr to keep track where the value is used.
  In Idris ``Ptr t`` is defined as external type.
- CFGCPtr The ``GCPtr t`` and ``GCAnyPtr`` are compiled to CFGCPtr. GCPtr has a special rule, it born
  from a Ptr value calling the ``onCollect`` function. The onCollect attaches a finalizer for the Ptr
  which should run when the pointer happens to be freed by the Garbage Collector of the Idris
  runtime. If there is no garbage collector, like in RefC back-end the finalizer should be called
  when the allocated memory for the value represented by the GCPtr gets freed.

These are the types that Idris communicates with Foreign codes, libraries in the host environment.
But let's step back and look into how this is represented at the Idris source level.
The simplest form of the FFI is the definition of a function with %foreign part. The %foreign part
as mentioned earlier it contains a list of strings that should be interpreted as by the code
generation back-end.

.. .code-block:: idris

  %foreign "C:add,libsmallc"
  prim__add : Int -> Int -> Int

This function refers the ``add`` function defined in the smallc.c file. The string after the foreign
is interpreted by the C back-end. In the FFI Int is considered to be CFInt. The back-end needs to
be sure that there is conversion between the representation of the types handled by the libraries
and the types represents Idris values.

.. .code-block:: idris

  data ThreadID : Type where [external]

  %foreign "scheme:blodwen-thread"
  prim__fork : (1 prog : PrimIO ()) -> PrimIO ThreadID

Here ThreadID is defined as external type and a ``CFUser "ThreadID" []`` description will be used
for the top-level definition of the prim__fork. The value which is created by the scheme
runtime it will be considered as a black box. The type of prim__fork is described
in the Foreign top-level definitions as ``[%World -> IORes Unit, %World] -> IORes Main.ThreadID``
Here we see that ``%World`` is added to the IO computations. The ``%World`` parameter is always the
last in the argument list.

For the FFI functions, the type information and the user defined string can be found in the top-level
definitions. The custom back-end should use that to generate a wrapper code, which should convert
the types that are described by the CFType to the types that the function in the code snippet needs.

Often there is a problem around Numeric Types and Strings in Idris. There is a design decision
has to be made here. In Idris there is no Float. From integers the 64Bits and arbitrary precision ones are supported,
From Word8 to Word64 are supported. String in Idris can not be Null. The decision here is how
to convert from these values to values of the functions written in the host language? Convert values
when precision is not adequate? Or stop compilation if such distinction is detected? What to do with
possibly null String values?

How to compile modules?
-----------------------

The Idris compiler generates intermediate files for modules, the content of the files are not the
Lifted, ANF, nor VMCode. Because of this, when the compilation pipeline enters the stage at code
generation all the information will be in one instance of the CompileData record and the custom
code generator back-end can process them as it would see the whole program.

Maybe the custom back-end wants to introduce some hierarchy for the functions in different
namespaces and organize some module structure to let the host technology process the bits and pieces
in different sized chunks, but this feature is not in scope of the Idris compiler.

How to embed code snippets?
---------------------------

One of the possible reasons to implement a custom back-end for Idris is to generate code for
another technology which has many libraries, but it doesn't have strong type properties.
There are classes of applications where strong types are necessary to guarantee properties
of software that shouldn't be broken from release to release. For example, software that
are responsible for lives of human beings. The new Idris compiler is standalone compiler
and compiles dependently typed programs fast it is able to fill the holes of software development
in the mission critical applications, even if there isn't too much libraries written in Idris yet.

When someone writes a custom back-end for this purpose the interoperability of the host technology
and the Idris based on the Foreign Interface can be not that convenient. In this situation
the code embedding of the host technology arises naturally. Elaboration can be an answer for that.

Elaboration is a compile time code generation. It uses the Elab monad which is part of the
type inference of the Idris compiler. With elaboration we can generate Idris code in Core.TT
format. When code snippets needs to be embedded a custom library should be provided with the
custom back-end that turns the valid code snippets to wrapping definitions into Core.TT
representation.

More on Elaboration_ can be found there.

What should the runtime system support?
---------------------------------------

As a summary, a custom back-end for the Idris compiler should create an environment
in the host technology that is able to run Idris programs. As Idris is part of
the family of functional programming languages, its computation model is based
on graph reduction. Programs represented as simple graphs in the memory based
on the closure creation mechanism during evaluation. Creating closures are
even at the lowest level of IRs can be found. For that reason any runtime in
any host technology needs to support some kind of representation of closures
and be able to store them on the heap, the responsibility of memory management
falls on the lap of the implementor of the custom back-end. If the host technology
has memory management, the problem is not that hard, also there is a big chance
that storing closures can be easily implemented via the tools of the host technology.

Although it is not clear what back-end should support. Tools from the Scheme back-end
are brought into the Idris world via external types and with there primitive operations
around them. This is a good practice and gives the community the ability to focus on
the implementation of a quick quick compiler for a dependently typed language.
One of these hidden features is the currency primitives. These are part of the
different libraries that could be part of the compiler or part of the
contribution package. If the threading model is different for the host technology
that the Idris inherits currently from the Scheme technology it could be a bigger
piece of work.

IO in Idris is implemented using an abstract ``%World`` value, which serves as token for
functions that operate interactively with the World through simple calls to the
underlying runtime system. The entry point of the program is the main, which
has the type of the IO unit, such as ``main : IO ()``. This means that every
program which runs, starts is part of some IO computation. Under the hood this is
implemented via creation of the ``%World`` abstract value, and invoking the main
function, which is compiled to pass the abstract %Wolre value for IO related
foreign or external operations.

There is an operation defined in the PrimIO module: ``unsafePerformIO``. Its
type signature tells that it is capable of evaluating an IO computation and
determining its result. Such as ``unsafePerformIO : IO a -> a``. The ``unsafePerformIO``
under the hood does exactly the same thing as the mechanism around the ``main`` does,
it invokes the creation of the abstract value ``%World`` and passes it to the
IO computations implicitly. This means there is a design decision here: How to
represent the state of the World, which part we are interested in, and how to
represent the world that is instantiated for the ``unsafePerformIO`` via the
``unsafeCreateWorld``? Both in the mechanism for main and the unsafeCreateWorld
uses the %MkWorld constructor, which will be compiled to the WorldVal and
its type to WorldType, which means the implementation of the runtime
is responsible for creating the abstraction around the World. Implementation of
abstract value World could be based on a singleton pattern, where we can have
just one world, or we could have more than one world, resulting parallel
universes for ``unsafePerformIO``.

Conclusion
==========

Implementing an Idris custom back-end is not that hard. It is a tedious work, but
many of the concepts are around how to implement a custom code generator of a
lambda calculus like language. If the implementation uses boxed primitive types,
storing constructor values on the heap will be uniform, that helps implementing or
inheriting the GC. The implementor of the custom back-end needs to decide
how deeply the different runtime futures needs to be supported.

If Idris is used as a sophisticated compiler for the domain, not full support
is necessary and libraries in Idris can be written that interface with the libraries
of the host technology and no concurrency primitives needs to be supported.
FFI can be implemented a way that the missing definitions can be read from an
external file, no need to wait for release processes of Idris libraries.

These properties makes the Idris compiler a really good fit for language oriented
architectures, where many languages are used on the same platform.

.. _SPLV20: https://www.youtube.com/playlist?list=PLmYPUe8PWHKqBRJfwBr4qga7WIs7r60Ql
.. _Elaboration: https://github.com/stefan-hoeck/idris2-elab-util/blob/main/src/Doc/Index.md
.. _Dart: https://github.com/bamboo/idris2dart
.. _Lua: https://github.com/Russoul/idris2-lua
.. _Erlang: https://github.com/idris-community/Idris2-Erlang
