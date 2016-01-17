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


## EDSL: Parts

<table class="table table-bordered">
<thead>
<tr>
    <th>EDSL Part</th>
    <th>Description</th>
</tr>
</thead>

<tr>
    <td class="success" > Types </td>
    <td class="alert-info" >  **Model** a concept </td>
</tr>

<tr>
    <td class="success" > Constructors </td>
    <td class="alert-info" >  **Construct** the simplest elements of such types </td>
</tr>

<tr>
    <td class="success" > Combinators </td>
    <td class="alert-info" >  **Build** complex elements from simpler ones </td>
</tr>

<tr>
    <td class="success" > Run functions </td>
    <td class="alert-info" >  **Observe** the EDSL elements, possibly producing
    side-effects when doing so </td>
</tr>
</table>

Let us get into a specific first in order to create a EDSL in Haskell.

## Shapes: a simple EDSL for vectorial graphics

* Design a language to describe *vectorial graphics*.
  - Made of some basic shapes.
  - They scale without loosing definition.
  - Ideal for logos.
  - Take less space as pixel-based formats (e.g., BMP, JPEG, etc.)

## Types and constructors

* We want to model simple graphics

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/basicshapes.png">
  </div>

    ```haskell
    newtype Shape
    empty  :: Shape
    disc   :: Shape
    square :: Shape  ```

## Some basic combinators

* Let us visualize some useful combinators

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/combinatorsshape.png">
  </div>

  ```haskell
  invert    :: Shape -> Shape
  intersect :: Shape -> Shape -> Shape
  translate :: ?
  ```

* What about `translate`?  We will move the figure based on a vector, i.e., on
  the x- and y-axes.

  ```haskell
  invert    :: Shape -> Shape
  intersect :: Shape -> Shape -> Shape
  translate :: Vec   -> Shape -> Shape
  ```
  <div class="alert alert-info">
  A new type (`Vec`) has appeared in the interface!
  </div>




## A run (observation) function

* A raster is going to render the image

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/raster.gif">
  </div>

* The raster is the one "observing" shapes and activating the corresponding
  pixels.

* We assume the rater is composed of points (pixels if you wish)

  ```haskell
  inside :: Point -> Shape -> Bool
  ```
  <div class="alert alert-info">
  A new type (`Point`) has appeared in the interface!
  </div>

## Shapes: the interface

* So far
  ```haskell
  newtype Shape

  empty     :: Shape
  disc      :: Shape
  square    :: Shape
  invert    :: Shape -> Shape
  intersect :: Shape -> Shape -> Shape
  translate :: Vec   -> Shape -> Shape
  inside    :: Point -> Shape -> Bool ```

* Aspects to think when designing an API
  - **Compositionality**: combining elements into more complex ones should be
    easy and natural.
  - ** Abstraction**: the user should not have to know (or be allowed to
    exploit) the underlying implementation of your types.

* Classification of operations
  - A **primitive operation** is defined exploiting the definitions of the
  involved types.
  - A **derived operation** is defined purely in terms of other operations.

## Implementation: shallow embedding

* **Types**: represent elements by their semantics, i.e., what they mean. It is usually
  done by leveraging some abstractions of the host language.
  ```haskell
  data Vec    = V { vecX, vecY :: Double }

  type Point  = Vec
  ptX = vecX
  ptY = vecY

  newtype Shape = Shape (Point -> Bool)
  ```
  In this case, we leverage Haskell functions!

* **Run** functions: they are almost for free!
  ```haskell
  inside :: Point -> Shape -> Bool
  p `inside` Shape sh = sh p
  ```

* **Constructor functions** and **combinators** do most of the work.

  ```haskell
   -- Constructors
   empty :: Shape
   empty = Shape $ \_ -> False

   disc :: Shape
   disc = Shape $ \p -> ptX p ^ 2 + ptY p ^ 2 <= 1

   square :: Shape
   square = Shape $ \p -> abs (ptX p) <= 1 && abs (ptY p) <= 1
  ```
  ```haskell
  -- Combinators
  invert :: Shape -> Shape
  invert sh = Shape $ \p -> not (inside p sh)

  intersect :: Shape -> Shape -> Shape
  intersect sh1 sh2 = Shape $ \p -> inside p sh1 && inside p sh2
  ```
  Translate deserves a bit of attention.

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/translateshape.png">
  </div>

  Observe that if a picture is moved `n` units to the left, then the
  characteristic functions should be applied to `x - n` instead.

  ```haskell
  sub :: Point -> Vec -> Point
  sub (V x y) (V dx dy) = V (x - dx) (y - dy)

  translate :: Vec -> Shape -> Shape
  translate v sh = Shape $ \p -> inside (p `sub` v) sh
  ```

## Transformation matrices
[Recommended reading](http://people.bath.ac.uk/sej20/transform.html)

* Basic idea

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/transform.png">
  </div>

* Let us see a concrete example: x-axis reflection

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/reflection.png">
  </div>

* Observe that a point appearing in the "screen" depends if such *point
  manipulated with some algebraic values* appears in the considered shape.


* We begin by introducing a new type for matrices and an inverse operation for
  them.

  ```haskell
  data Matrix = M Vec Vec

  inv :: Matrix -> Matrix
  inv (M (V a b) (V c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
   where k = a * d - b * c
  ```

* Now the `transform` operation
  ```haskell
  transform :: Matrix -> Shape -> Shape
  transform m sh = Shape $ \p -> (inv_m `mul` p) `inside` sh
     where inv_m = inv m
  ```

* Exercise: can you write the following **derived** operations?
  ```haskell
  -- reflects the shape on the x-axis
  x-reflection :: Shape -> Shape

  -- reflects the shape on the y-axis
  y-reflection :: Shape -> Shape

  -- Enlarge the shape by n% (n being the first argument)
  zoom_in :: Int -> Shape -> Shape
  ```



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
