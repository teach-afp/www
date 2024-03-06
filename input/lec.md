## 1. [Introduction to the course](./lecture1.html).

* **Topics**: The goal of the course. Course Materials. Organization. Evaluation
  orders. Lazyness. Type classes.

* **Reading**: Chapters 1&ndash;6, Chapters 8&ndash;10 (mostly
repetition of what you have seen in Introduction to Functional Programming at
Chalmers). Note that it is ~200 pages, so start reading now if you need an
update!

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L1/)

* **Optional reading**:
  - Applicative Functors are introduced in Real World Haskell Chapter 10, used
    more later in the same book Chapter 16.
  - Haskell &mdash; The Craft of Functional Programming: Chapter 12 on overloading, Chapter 16 on
    abstract types, and Chapter 17 on laziness.
  - The Haskell School of Expression: Chapter 12 on type classes, Chapter 14 on
    streams, Section 18.1 on higher-order types.

## 2. [Domain Specific Embedded Languages (EDSL)](./lecture2.html).

* **Topics**: Developing EDSL for describing shapes (e.g., squares, disc, etc.)
    and signals (to represent change of values over time). Shape animation. Deep
    vs. shallow embedding. Compositionality and abstraction.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L2/)

* **Reading**:
    [DSL for the Uninitiated by Debasish Ghosh](http://cacm.acm.org/magazines/2011/7/109910-dsl-for-the-uninitiated/fulltext),
    Chapter 5 has a EDSL for pretty printing, Chapter 9 for file searching and
    Chapter 13 one for arithmetic expressions.

* **Exercises**:
  - Shape: extend the library with colored shapes
  - Shape: define derived opertations `x-reflection`, `y-reflection`, and `zoom_in`
  - Signal: define `mapS` as a derived operation

## 3. [Monads](./lecture3.html)

* **Topics**: Side-effects in pure functional programming. Monads. Monads for
    error handling, logging, and state. Monads and EDSL.

* **Reading**: Chapter 14

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L3/Interpr.hs)

* **Optional reading**:
  - Chapter 15
  - [Monads for functional programming by Philip Wadler](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
     (the interpreter for arithmetic expressions is taken from this paper)

## 4. [Functors, Applicative Functors, and Monads](./lecture4.html)

* **Topics**: Functors. Applicative functors. Relation among functors,
    applicative functors, and monads. More example of monads (modeling IO).

* **Reading**: Chapter 10 (Section Introducing Functors),
  [FUNCTIONAL PEARL Applicative programming with effects (Page 1 &ndash; 6)](http://strictlypositive.org/IdiomLite.pdf)

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L4/)

* **Optional reading**:
  - Functors: [Page 1 and 2 from "Functional Pearl: F for Functor" by R. Hinze,
     J. Hackett, and D. W. H. James](http://www.cs.ox.ac.uk/people/daniel.james/functor/functor.pdf)
  - Applicative functors:
    [FUNCTIONAL PEARL Applicative programming with effects](http://strictlypositive.org/IdiomLite.pdf)
  - Blog on [Functors are containers by B. Milewski](http://bartoszmilewski.com/2014/01/14/functors-are-containers/).
    This blog post explains Functors, Applicative Functors, and Monads using the
    concept of containers.
  - Intermediate embedding:
    [Beauty in the Beast, A Functional Semantics for the Awkward Squad by W. Swierstra and T. Altenkirch](http://www.cs.nott.ac.uk/~psztxa/publ/beast.pdf)
    Sections 1, 2 and 3.

## 5. [Parser derivation](./lecture5.html)

* **Topics**: Another application of monads, namely parsing. Refinement of
    implementation by program derivation. The focus here is on learning outcome
    "Spec: use specification based development techniques".

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L5/)

* **Reading**: Chapter 16, until p. 390 or so; to get a feel for what parser
    combinators are,
    [Parallel Parser Combinators](./assets/files/parser-claessen.pdf),
    [Koen's slides](./assets/files/afp-2015-lecture4.pdf)

* **Optional reading**:

  - [Monadic Parser Combinators](./assets/files/parser-hutton.ps)
    by Graham Hutton and Erik Meijer.

  - [The Design of a Pretty-printing Library](./assets/files/pretty-hughes.ps)
     by John Hughes.


## 6. [Monad transformers](./lecture6.html)

* **Topics**: we learn how to build complicated monads from simple building
    blocks. We cover the reader and state monad transformers. We apply them to
    write a interpreter for simple expressions.

* [Source
  code](https://github.com/teach-afp/afp-code/blob/master/L6/)

* **Reading**: Chapter 18 on Monad Transformers.

* **Optional reading**:

  - [Monad Transformers and Modular Interpreters](./assets/files/modular-interpreters-liang.ps)
    by Sheng Liang, Paul Hudak, and Mark Jones.

## 7. [Monad transformers II](./lecture7.html)

* **Topics**: This lecture looks into the consequences of the application order
    of `StateT` and `ExceptT` monad transformers in the interpreter. The lecture
    also shows how to create your own monad transformers &mdash; for that, we show an
    implementation for `StateT`, `ExceptT`, and `ReaderT`.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L7/)

* The reading material is the same as lecture 6 (see above).


## 8. [Program verification by equational reasoning](./lecture8.html)

* **Topics**: In this lecture, we look at program verification by proving
    properties by induction.  We do this semi-formally in Haskell first
    and then fully formally in Agda.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L8/)

* **Reading**: Chapter 11. Chapter 2 in the book *The Fun of Programming*.

* **Optional reading**: [QuickCheck: A Lightweight Tool for Random Testing of
    Haskell Programs by K. Claessen and
    J. Hughes](./assets/files/QuickCheck-claessen.ps)


## 9. Data structure invariants in Agda I: Ordered search trees

* **Topics**: We review Agda syntax and the Curry-Howard Correspondence
  that allows us to program proofs.
  We then implement the tree sort algorithm in Agda using binary search trees
  that are correctly ordered by construction.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L9-TreeSort/)

* **Optional reading**:
  [How to Keep your Neighbors in Order, by C. McBride](https://personal.cis.strath.ac.uk/conor.mcbride/Pivotal.pdf)


## 10. Data structure invariants in Agda II: Balanced trees

* **Topics**: We review 2-3 Trees and implement them in Agda
  such that they are correctly balanced by construction.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L10/)

* The reading material is the same as for the previous lecture.


## 11. Data structure invariants in Agda III: Typed syntax trees

* **Topics**: We implement a type checker and interpreter for a simple expression language in Agda.
  The type checker produces syntax trees can only represent only well-typed programs.
  An interpreter for such well-typed syntax trees can use tagless values.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L11/)


## 12a. [Type-based modeling in Haskell I](./lecture-type-based-modeling-i.html)

* **Topics**: We look how to do type-inference (type-checking) in DSL using
    GADTs. We describe existential and singleton types.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/L12/)
* [Source code](https://github.com/teach-afp/afp-code/blob/master/TypeBasedModelingI/)

* **Reading**: [Existentially qualified types](https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types)
    and [GADTs](https://wiki.haskell.org/Generalised_algebraic_datatype).


## 12b. [Type-based modeling in Haskell II](./lecture-type-based-modeling-ii.html)

* **Topics**: We will discuss associated
    types, kinds, data kinds, type families, and singleton types.

* [Source code](https://github.com/teach-afp/afp-code/blob/master/TypeBasedModelingII/)

* **Reading**: [Type families](https://wiki.haskell.org/GHC/Type_families) on the
    Haskell Wiki, and Associated types in
    [S. Peyton-Jones' slides](http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/FunWithTypeFuns-Apr09.pdf)

* **Optional reading**: [Fun with Type Functions](https://wiki.haskell.org/Simonpj/Talk:FunWithTypeFuns)
    by O. Kiselyov, S. Peyton-Jones, and C. Shan


## 13 & 14. [Software engineering in Haskell](./lecture-packaging.html)

* **Topics**: We discuss software engineering in Haskell:
  - Writing package manifests (`.cabal` files).
  - The Haskell [Package Version Policy](https://pvp.haskell.org)
  - Testsuites:
    * Unit tests with package `tasty-hunit`.
    * Property tests with package `tasty-quickcheck`.
    * Tested documentation with package `doctest`.
  - Continuous Integration on `https://github.com`.
  - Resilient coding style.

* [Source code](https://github.com/teach-afp/binary-search-trees)


## 15. [Information-flow Control in Haskell](./lecture-security.html)

* **Topics**: In this lecture, we will see a use of monads for controlling
    effects in order to provide security for Haskell programs. We will focus on
    how to preserve sensitive data when executed by untrusted code, i.e., code
    written by someone else.

* [Source code for MAC library](https://bitbucket.org/russo/mac-lib),
  [Source code for the demo](https://bitbucket.org/russo/mac-demo)


* **Reading**: Functional Pearl:
  [Two can keep a secret, If one of them uses Haskell by A. Russo](http://www.cse.chalmers.se/~russo/publications_files/pearl-russo.pdf)
