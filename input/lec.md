We will try to have the lecture notes available on-line before the lecture so
you can bring them to the lecture to do notes and (in case of errors)
corrections. However, *it might not happen* since there are some changes for
this year. For impatient students, you could check last [year's web
page](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/).

## 1. [Introduction to the course](./lecture1.html).

* **Topics**: The goal of the course. Course Materials. Organization. Evaluation
  orders. Lazyness. Type classes.

* **Reading**: Chapters 1-6, Chapters 8-10 (mostly
repetition of what you have seen in Introduction to Functional Programming at
Chalmers). Note that it is ~200 pages, so start reading now if you need an
update!

* [Source code](https://bitbucket.org/russo/afp-code/src/1f5af2807b33b925059be1380ffe7e1049fbb9cb/L1/?at=master)

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

* [Source code](https://bitbucket.org/russo/afp-code/src/1f5af2807b33b925059be1380ffe7e1049fbb9cb/L2/?at=master)

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
  code](https://bitbucket.org/russo/afp-code/src/a581a6b1208c6ab826276bd26ad95f90e1819865/L3/Interpr.hs?at=master&fileviewer=file-view-default)

* **Optional reading**:
  - Chapter 15
  - [Monads for functional programming by Philip
     Wadler](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
     (the interpreter for arithmetic expressions is traken from this paper)

## 4. [Functors, Applicative Functors, and Monads](./lecture4.html)

* **Topics**: Functors. Applicative functors. Relation among functors,
    applicative functors, and monads. More example of monads (modeling IO).

* **Reading**: Chapters 10 (Section Introducing Functors),

* [Source
  code]()

* **Optional reading**:
  - [Page 1 and 2 from "Functional Pearl: F for Functor" by R. Hinze,
     J. Hackett, and D. W. H. James](http://www.cs.ox.ac.uk/people/daniel.james/functor/functor.pdf)
