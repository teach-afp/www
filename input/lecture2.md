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
  - In this course, we use Haskell as the host language
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

## Shape rotation

* Axes rotation (math revisited)

  <div class="container">
     <img class="img-responsive col-md-8"
       src="./assets/img/rotation.png">
  </div>

* Which direction is the square shape rotated?
  ```haskell
  transform (m (pi/10)) square
    where m alpha = matrix
                       (cos alpha)    (sin alpha)
                       (-(sin alpha)) (cos alpha) ```

  Clock-wise! Can you see why?

## Points, Vectors, and Matrices in a separate module

* The interface for shapes uses auxiliary mathematical abstractions

* We define them in a different module (`Matrix.hs`)

## Alternative implementation : deep embedding

* **Types**: represent how shapes are constructed (i.e., either by a basic shape
  or a combination of them).
  ```haskell
  data Shape where
    -- Constructor functions
    Empty   :: Shape
    Disc    :: Shape
    Square  :: Shape
    -- Combinators
    Translate :: Vec ->   Shape -> Shape
    Transform :: Matrix-> Shape -> Shape
    Intersect :: Shape -> Shape -> Shape
    Invert    :: Shape -> Shape
  ```

* **Constructors** and **combinators**: almost for free! Simply map them into
    the appropriated constructors in the data type.

  ```haskell
  empty  :: Shape
  empty  = Empty

  disc   :: Shape
  disc   = Disc

  square :: Shape
  square = Square

  transform :: Matrix -> Shape -> Shape
  transform = Transform

  translate :: Vec -> Shape -> Shape
  translate = Translate

  intersect :: Shape -> Shape -> Shape
  intersect = Intersect

  invert :: Shape -> Shape
  invert = Invert
  ```

* **Run** (observation) function: all the work is here!

  ```haskell
   inside :: Point -> Shape -> Bool
   _p `inside` Empty             = False
   p  `inside` Disc              = sqDistance p <= 1
   p  `inside` Square            = maxnorm  p <= 1
   p  `inside` Translate v sh    = (p `sub` v) `inside` sh
   p  `inside` Transform m sh    = (inv m `mul` p) `inside` sh
   p  `inside` Union sh1 sh2     = p `inside` sh1 || p `inside` sh2
   p  `inside` Intersect sh1 sh2 = p `inside` sh1 && p `inside` sh2
   p  `inside` Invert sh         = not (p `inside` sh)

   -- * Helper functions
   sqDistance :: Point -> Double
   sqDistance p = x*x+y*y -- proper distance would use sqrt
     where x = ptX p
           y = ptY p

   maxnorm :: Point -> Double
   maxnorm p = max (abs x) (abs y)
     where x = ptX p
           y = ptY p
   ```

## Shallow vs. Deep embedding

* A shallow embedding (when it works out) is often more elegant and compact.
  - Working out the type which provides the right semantics might be tricky.

* A deep embedding is easier to extend.
  - Adding new operations (by adding new constructors).
  - Adding new run functions.
  - Adding optimizations (e.g., by data type manipulation).

* Most of the time you get a *mixed* between shallow and deep embedding.

* In any case, **abstraction** is important!

  ```haskell
  module Shape.Shallow
  (
    -- * Types
    Shape -- abstract
    -- * Constructor functions
  , empty, disc, square
    -- * Primitive combinators
  , transform, translate
  , union, intersect, invert
    -- * Run functions
  , inside
  )
  where ...
  ```
  The interface should not change based on a shallow or deep embedding
  implementation! No difference for the user of the EDSL!

## Rendering a shape to ASCII-art

* A very interesting *run function*
* It introduces the concept of windows
  ```haskell
  defaultWindow :: Window
  defaultWindow = Window
    { bottomLeft  = point (-1.5) (-1.5)
    , topRight    = point 1.5 1.5
    , resolution  = (40, 40)
    }
  ```
  - It has a dimension in terms of characters
  - It has a dimension in terms of points

* The render function essentially maps points to characters in a window.

  ```haskell
  -- | Generate a list of evenly spaces (n :: Int) points in an interval.
  samples :: Double -> Double -> Int -> [Double]

  -- | Generate the matrix of points corresponding to the pixels of a window.
  pixels :: Window -> [[Point]]

  -- | Render a shape in a given window.
  render :: Window -> Shape -> String
  render win sh = unlines $ map (concatMap putPixel) (pixels win)
    where
      putPixel p  | p `inside` sh = "[]"
                  | otherwise     = "  "
  ```

  - Function `unlines` and `concatMap` are standard.


## Signal: Another EDSL

* We are interested to produce animated shapes.
* For that, we need to model how they might change based on time.
* We take inspiration from [functional reactive
  programming](https://wiki.haskell.org/Functional_Reactive_Programming#Publications_and_talks)
  approaches.
* We introduce the concept of `Signal`, a value that changes over time.
* More specifically, we have the following API for our new EDSL.
  ```haskell
  -- Constructors
  constS :: a -> Signal a
  timeS  :: Signal Time
  -- Combinators
  ($$)   :: Signal (a -> b) -> Signal a -> Signal b
  mapT   :: (Time -> Time)  -> Signal a -> Signal a
  -- Derived operation
  mapS   :: (a -> b) -> Signal a -> Signal b
  -- Run function
  sample :: Signal a -> Time -> a
  ```



## Discussion

* Adding colored shapes
  - Discuss what you need to do
* Bad shallow implementations
  - Looking at the render run function, we might decide to go for
    ```haskell
    newtype Shape = Shape (Window -> String)
    ```
  - Discuss the problem with this implementation
* Other questions/comments?

## Summary

* Different kind of operations
  - Constructors, combinators, and run functions
  - Primive or derived operations
* Implementation styles
  - Shallow embedding: representation given by semantics
  - Deep embedding: representation given by operations
* Remember
  - Compositionality
  - Abstraction
