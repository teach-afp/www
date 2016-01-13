# Domain Specific Embedded Languages

## A common problem in software development
[Reading: DSL for the Uninitiated by Debasish Ghosh](http://cacm.acm.org/magazines/2011/7/109910-dsl-for-the-uninitiated/fulltext)
* Business users speak the domain terminology
  - Stock market: bonds, stocks, options, etc.
  - Cryptography: symmetric keys, asymmetric keys, games, perfect secrecy, chain
    blocked cypher, etc.
  - Computer graphics: vectors, bézier curve, line, bézier spline, etc.
* Developers speak software language
  - Data types
  - Functions
  - Modules
  - Lazy evaluation
* There is a semantic gap.

  <img class="img-thumbnail"
     src="./assets/img/semgap.png"
     height="70%"
     width="70%"
     style="float:left" >

   <!-- Trick to avoid wrapping around more text than it should -->
   <div class="row">

   </div>
* How can we bridge the gap?
  - Modeling the domain in software.
  - In other words, we should aim to design a **domain model** in your software.
* How do we start? What if the domain is colossal?
  - Identify **minimum** domain model elements.
  - Propose constructs that **glue** together domain model elements and create
    other (possible more complex) ones.
  - Define how **minimum** and **glued** elements behave in your software.
* The **domain model elements** and the corresponding **constructs** are the
  common language between the developer and the business users.

   <img class="img-thumbnail"
     src="./assets/img/nosemgap.png"
     height="70%"
     width="70%"
     style="float:left" >

   <!-- Trick to avoid wrapping around more text than it should -->
   <div class="row">
* It is the *domain specific language* (or DSL) which brings the gap between
  business users and developers.

## EDSL vs. DSL

* What is the different between DSL and embedded DSL (or EDSL)?
* A domain specific language can be provided as stand alone.
  - You write a compiler/interpreter for the DSL and provide all the tools to
    program with it
* In contrast, an embedded DSL is written in a host language.
  - It inherits the infrastructure (and programming language abstractions) of
    the host language.
  - In this course, we use Haskell as the host language.
  - We will leverage Haskell
    * powerful type system,
    * generic programming features,
    * and tools

    for programming our DSL.
* Many abstractions used in functional programming (e.g., monads) are known for
  being suitable to implement EDSLs. (We will see many examples along the
  course)


## Rasterific: A EDSL for drawing

[Rasterific in Hackage](http://hackage.haskell.org/package/Rasterific-0.6.1/docs/Graphics-Rasterific.html)

* Install the Hackage package
  ```bash
  cabal install rasterific
  ```
* Rasterific is designed to talk the language of *vectorial graphics*.
  - Made of lines and paths.
  - They scale without loosing definition.
  - Ideal for logos.
  - Take less space as pixel-based formats (e.g., BMP, JPEG, etc.)

*
