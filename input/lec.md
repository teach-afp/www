We will try to have the lecture notes available on-line before the lecture so
you can bring them to the lecture to do notes and (in case of errors)
corrections. However, *it might not happen* since there are some changes for
this year. For impatient students, you could check [last year's web
page](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/).

## 1. [Introduction to the course](./lecture1.html).

* **Topics**: The goal of the course. Course Materials. Organization. Evaluation
  orders. Lazyness. Type classes.

* **Reading**: Chapters 1-6, Chapters 8-10 (mostly
repetition of what you have seen in Introduction to Functional Programming at
Chalmers). Note that it is ~200 pages, so start reading now if you need an
update!

* [Source code](https://bitbucket.org/russo/afp-code/src/HEAD/L1/?at=master)

* **Optional reading**:
  - Applicative Functors are introduced in Real World Haskell Chapter 10, used
    more later in the same book Chapter 16.
  - Haskell -- The Craft of Functional Programming: Chapter 12 on overloading, Chapter 16 on
    abstract types, and Chapter 17 on laziness.
  - The Haskell School of Expression: Chapter 12 on type classes, Chapter 14 on
    streams, Section 18.1 on higher-order types.

## 2. [Domain Specific Embedded Languages (EDSL)](./lecture2.html).

* **Topics**: Developing EDSL for describing shapes (e.g., squares, disc, etc.)
    and signals (to represent change of values over time). Shape animation. Deep
    vs. shallow embedding. Compositionality and abstraction.

* [Source code](https://bitbucket.org/russo/afp-code/src/HEAD/L2/?at=master)

* **Reading**: [DSL for the Uninitiated by Debasish
    Ghosh](http://cacm.acm.org/magazines/2011/7/109910-dsl-for-the-uninitiated/fulltext),
    Chapters 5 has a EDSL for pretty printing, Chapter 9 for file searching and
    Chapter 13 one for arithmetic expressions.

* **Exercises**:
  - Shape: extend the library with colored shapes
  - Shape: define derived opertations `x-reflection`, `y-reflection`, and `zoom_in`
  - Signal: define `mapS` as a derived operation

## 3. [Monads](./lecture3.html)

* **Topics**: Side-effects in pure functional programming. Monads. Monads for
    error handling, logging, and state. Monads and EDSL.

* **Reading**: Chapters 14

* [Source
  code](https://bitbucket.org/russo/afp-code/src/HEAD/L3/Interpr.hs?at=master&fileviewer=file-view-default)

* **Optional reading**:
  - Chapter 15
  - [Monads for functional programming by Philip
     Wadler](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
     (the interpreter for arithmetic expressions is taken from this paper)

## 4. [Functors, Applicative Functors, and Monads](./lecture4.html)

* **Topics**: Functors. Applicative functors. Relation among functors,
    applicative functors, and monads. More example of monads (modeling IO).

* **Reading**: Chapters 10 (Section Introducing Functors), [FUNCTIONAL PEARL
    Applicative programming with effects (Page 1 -
    6)](http://strictlypositive.org/IdiomLite.pdf)

* [Source
  code](https://bitbucket.org/russo/afp-code/src/HEAD/L4/?at=master)

* **Optional reading**:
  - Functors: [Page 1 and 2 from "Functional Pearl: F for Functor" by R. Hinze,
     J. Hackett, and D. W. H. James](http://www.cs.ox.ac.uk/people/daniel.james/functor/functor.pdf)
  - Applicative functors: [FUNCTIONAL PEARL Applicative programming with
  effects](http://strictlypositive.org/IdiomLite.pdf)
  - Blog on [Functors are containers by
    B. Milewski](http://bartoszmilewski.com/2014/01/14/functors-are-containers/). This
    blog post explains Functors, Applicative Functors, and Monads using the
    concept of containers.
  - Intermediate embedding: [Beauty in the Beast, A Functional Semantics for
    the Awkward Squad by W. Swierstra and
    T. Altenkirch](http://www.cs.nott.ac.uk/~psztxa/publ/beast.pdf) Sections 1, 2
    and 3.

## 5. [Parser derivation](./lecture5.html)

* **Topics**: Another application of monads, namely parsing. Refinement of
    implementation by program derivation. The focus here is on learning outcome
    "Spec: use specifictaion based development techniques".

* [Source
  code](https://bitbucket.org/russo/afp-code/src/HEAD/L5/?at=master)

* **Reading**: Chapters 16, until p. 390 or so; to get a feel for what parser
    combinators are, [Parallel Parser
    Combinators](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/Papers/parser-claessen.pdf),
    [Koen's slides](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/lectures/lecture4/lecture4.pdf)

* **Optional reading**:

  - [Monadic Parser
    Combinators](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/Papers/parser-hutton.ps)
    by Graham Hutton and Erik Meijer.

  - [The Design of a Pretty-printing
     Library](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/Papers/pretty-hughes.ps)
     by John Hughes.


## 6. [Monad transformers](./lecture6.html)

* **Topics**: we learn how to build complicated monads from simple building
    blocks. We cover the reader and state monad transformers. We apply them to
    write a interpreter for simple expressions.

* [Source
  code](https://bitbucket.org/russo/afp-code/src/HEAD/L6/?at=master)

* **Reading**: Chapters 18 on Monad Transformers.

* **Optional reading**:

  - [Monad Transformers and Modular
    Interpreters](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/Papers/modular-interpreters-liang.ps)
    by Sheng Liang, Paul Hudak, and Mark Jones.

## 7. Looking forward! (February 11th)

* **Topics**: In this lecture / excercise session we will work through old AFP
    exam questions in groups to identify important topics and practice
    collaborative problem solving and discussion. This is in response to student
    comments wanting more practice of the kind of problems typically included in
    the written exams.

* **Assistants on charge**: Dan Rosén and Anton Ekblad

## 8. [Monad transformers II](./lecture8.html)

* **Topics**: This lecture looks into the consequences of the application order
    of `StateT` and `ExceptT` monad transformers in the interpreter. The lecture
    also shows how to create your own monad transformers -- for that, we show an
    implementation for `StateT`, `ExceptT`, and `ReaderT`.

* [Source
  code](https://bitbucket.org/russo/afp-code/src/HEAD/L8/?at=master)

* The reading material is the same as lecture 6 (see above).

## 9. [Information-flow Control in Haskell](./lecture9.md)

* **Topics**: In this lecture, we will see a use of monads for controlling
    effects in order to provide security for Haskell programs. We will focus on
    how to preserve sensitive data when executed by untrusted code, i.e., code
    written by someone else.

* [Source
  code for MAC library](https://bitbucket.org/russo/mac-lib), [Source code for
  the demo](https://bitbucket.org/russo/mac-demo)


* **Reading**: Functional Pearl: [Two can keep a secret, If one of them uses
    Haskell by A. Russo](http://www.cse.chalmers.se/~russo/publications_files/pearl-russo.pdf)


## 14. Looking forward! (March 3rd)

* **Topics**: Second instance of the excercise session with exam questions.

## 16. Looking back! (March 10th)

* **Topics**: In this lecture we briefly look back at the learning outcomes and
    how they relate to the different parts of the course and what parts of the
    Real world Haskell book are covered. Then we go through a few examples
    chosen by popular vote by the participants.

* **Assistants on charge**: John J. Camilleri and Anton Ekblad
