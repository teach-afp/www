# Functors, Applicative, and Monads

## Monads

* A structure that represents computations defined as sequences of steps.
  <div class = "alert alert-info">
  The bind operator `(>>=)` defines what it means to chain operations of a
  monadic type.
  </div>

* In this lecture, we will learn about two other structures useful in functional
  programming
  - Functors
  - Applicative functors

## Structure-preserving mappings

* We are familiar with the `map` function over lists
  ```haskell
  map (+1) [2,3,4,5,6] ```
  which *applies the function `(+1)` to every element of the list*, and produces
  the list
  ```haskell [3,4,5,6,7]
  ```

* We can generalize the concept of `map` to work on trees

  ```haskell
  data Tree a = Leaf a | Node (Tree a) (Tree a)

  mapTree :: (a -> b) -> Tree a -> Tree b
  mapTree f (Leaf a) = Leaf (f a)
  mapTree f (Node t1 t2) = Node (mapTree f t1)
                                (mapTree f t2) ```

* As we did with lists, we can apply the function `(+1)` at every element of the
  data structure.

  For instance,
  ```haskell
  mapTree (+1) (Node (Leaf 2) ((Node (Node (Leaf 3) (Leaf 4)) (Leaf 5))))
  ```
  produces the following tree.
  ```haskell
  Node (Leaf 3) (Node (Node (Leaf 4) (Leaf 5)) (Leaf 6)) ```

* In both cases, the structure of the data type (i.e., lists and trees) is preserved

* General pattern:

  <div class="alert alert-info">
  It is useful to apply functions to data types elements while respecting the
  structure (shape) of it
  </div>

  <div class="container">
     <img class="img-responsive col-md-10"
       src="./assets/img/functor.png">
  </div>


## Functors

* A functor is composed of two elements: a *data type definition* (*container*)
  and a generalized `map`-function called `fmap`

* In Haskell, `fmap` is an overloaded function, i.e., defined for every
  container which support a `map`-like operation

  ```haskell
  class Functor d where
     fmap :: (a -> b) -> d a -> d b
  ```

* A functor must obey the following laws.

  <table class="table table-bordered">
  <thead>
    <tr>
    <th>Name</th>
    <th>Law</th>
    </tr>
  </thead>

  <tr>
  <td> Identity: </td>
  <td>  ```haskell fmap id ≡ id ```
  </td>
  </tr>

  <tr>
  <td> Map fusion (or composition): </td>

  <td> ```haskell fmap (f . g) ≡ fmap f . fmap g ```
  </td>
  </tr>
  </table>
  Observe that *map fusion* allows to reduce two traversals to one!

* Let us consider again the `Tree` example

  ```haskell
  instance Functor Tree where
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)
  ```
  Now, we can write
  ```haskell
  (+1) `fmap` (Node (Leaf 2) ((Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)))) ```

## Functors: more examples

* `Maybe` data type

   ```haskell
   (+1) `fmap` (Just 10)
   ```
* Generalized trees

  ```haskell
  data TreeG a = LeafG a | BranchG [TreeG a]

  instance Functor TreeG where
   fmap f (LeafG a)    = LeafG (f a) ```

  What about `BranchG`? We will use the `fmap` from lists!

  ```haskell
  fmap f (BranchG ts) = BranchG (fmap (fmap f) ts)
  ```
  The outermost `fmap` is the one corresponding to lists.

  For instance, the expression
  ```haskell
  (+1)
  `fmap` BranchG [LeafG 10,
                  BranchG [LeafG 11, LeafG 12],
                  BranchG [LeafG 13, LeafG 14, LeafG 15]] ```
  produces
  ```haskell
  BranchG [LeafG 11,BranchG [LeafG 12,LeafG 13],BranchG [LeafG 14,LeafG 15,LeafG 16]]
  ```

* To summarize:

  <div class = "alert alert-info">
  Functors simplifies boilerplate code, i.e., destructing a container in order
  to create another one!
  </div>

  <div class = "alert alert-info">
  As data types can be built from other ones, so do functors! (thus achieving modularity)
  </div>

## Not a functor

* Not every data type is a functor

  ```haskell
   type NotAFunctor = Equal
   newtype Equal a = Equal {runEqual :: a -> a -> Bool}

   fmapEqual :: (a -> b) -> Equal a -> Equal b
   fmapEqual _f (Equal _op) = Equal $ \_b1 _b2 -> error "Hopeless!" ```

* What is the problem?

  Values of type `a` are in "negative position", i.e., they are given to the
  function (not produced by it).

## Multi-parameter functions applied to multiple containers

* In a more general case, sometimes we would like to transform elements in a
  container (data type) based on applying a "*multi-parameter function*" to
  *multiple containers* (the quotes are there since in Haskell every function
  has exactly one argument due to curryfication)

* Keep in mind that `fmap` takes a function of type `a -> b` and not
  "multi-parameter" ones, e.g., `a -> b -> c`

* Let us take as an example to see what happens when `fmap` is used with a
  "multi-parameter" function.

  ```haskell
   mp_fmap :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
   mp_fmap f ma mb = let m_b_to_c = fmap f ma
                     in  undefined ```

  Once we apply `fmap` to the first container, we have a container (`m_b_to_c`)
  with a function of type `b -> c` in it.

  To keep applying `f`, what we need to do is to **take its partial application
  out of the container `m_b_to_c` and mapped over `mb`**, where its
  next argument is located.

  ```haskell
  mp_fmap :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  mp_fmap f ma mb = let m_b_to_c = fmap f ma
                    in case m_b_to_c of
                            Nothing -> Nothing
                            Just fa -> fmap fa mb ```

  What happens if function `f` had more arguments?

  ```haskell
  mp_fmap2 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
  mp_fmap2 f ma mb mc = case fmap f ma of
                             Nothing -> Nothing
                             Just fa -> case fmap fa mb of
                                            Nothing  -> Nothing
                                            Just fab -> fmap fab mc ```

  We would need to implement more code to extract partial application of
  functions out from containers, applied them, and wrap the result in another
  container!

  *Applicative functors are conceived to do precisely this work!*


## Applicative functors

* An *applicative functor* (or functor with application) is a functor with the following operations

  ```haskell
  class Applicative d where
        pure  :: a -> d a
        (<*>) :: d (a -> b) -> d a -> d b  ```

* The pure operation creates a container with the given argument. The interesting
   operation is "application" `(<*>)`, which we can describe it graphically as
   follows

  <div class="container">
     <img class="img-responsive col-md-9"
       src="./assets/img/applicative.png">
  </div>

  It takes a container of functions and a container of arguments and returns a
  container of the result of applying such functions.

*

## Relation with monads

## A monad for I/O
