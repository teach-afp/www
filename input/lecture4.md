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

[Page 1 and 2 from "Functional Pearl: F for Functor" by R. Hinze,
J. Hackett, and D. W. H. James](http://www.cs.ox.ac.uk/people/daniel.james/functor/functor.pdf)

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


## Functors


## A bit of category theory

## Not a functor

* Not every data type is a functor

## Applicative Functors


## Relation with monads

## A monad for I/O
